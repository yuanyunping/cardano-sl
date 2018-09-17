{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

import qualified Data.List.NonEmpty as NonEmpty
import           Test.QuickCheck (Property, Result (..), conjoin, discard,
                     ioProperty, label, property, quickCheckResult,
                     withMaxSuccess, (===))

import           Cardano.X509.Configuration (CertDescription (..),
                     DirConfiguration (..), ErrInvalidAltName,
                     ErrInvalidExpiryDays, ServerConfiguration (..),
                     TLSConfiguration (..), fromConfiguration, genCertificate)
import           Data.X509.Extra (genRSA256KeyPair, validateSHA256)
import           Test.Cardano.X509.Configuration.Arbitrary (Invalid (..))


--
-- Main
--

main :: IO ()
main = runQuickCheck
    [ quickCheckResult $ label "GenCertificate is Valid" propGenCertificateValid
    , quickCheckResult $ label "Invalid AltDNS throws" propInvalidAltDNS
    , quickCheckResult $ label "Invalid Expiry Days throws" propInvalidExpiryDays
    ]
  where
    -- NOTE running 'quickCheck prop' doesn't make 'cabal test' fails
    -- even if the property fails. So this little one cope with this
    -- by running all specs and failing if one of them returned a failure.
    runQuickCheck :: [IO Result] -> IO ()
    runQuickCheck =
        sequence >=> (mapM_ $ \case
            Success {} -> return ()
            _          -> exitFailure)


--
-- Properties
--

-- | Verify that each certificate generated is valid. Is uses the default
-- validation check of 'Data.X509.Validation'
propGenCertificateValid
    :: (TLSConfiguration, DirConfiguration)
    -> Property
propGenCertificateValid =
    ioProperty . generateAndValidate


-- | Verify that we can't generate certificates when provided an invalid
-- list of alternative names as IP addresses.
propInvalidAltDNS
    :: (Invalid TLSConfiguration, DirConfiguration)
    -> Property
propInvalidAltDNS (Invalid tlsConf, dirConf) =
    withMaxSuccess 10 $ ioProperty $ generateAndValidate (tlsConf, dirConf)
        `catch` (\(_ :: ErrInvalidAltName)    -> return $ property True)
        `catch` (\(_ :: ErrInvalidExpiryDays) -> discard)
        `catch` (\(e :: SomeException)        -> throwM e)


-- | Verify that we can't generate certificates when provided invalid
-- expiry days.
propInvalidExpiryDays
    :: (Invalid TLSConfiguration, DirConfiguration)
    -> Property
propInvalidExpiryDays (Invalid tlsConf, dirConf) =
    withMaxSuccess 10 $ ioProperty $ generateAndValidate (tlsConf, dirConf)
        `catch` (\(_ :: ErrInvalidAltName)    -> discard)
        `catch` (\(_ :: ErrInvalidExpiryDays) -> return $ property True)
        `catch` (\(e :: SomeException)        -> throwM e)


-- | Actually generate certificates and validate them. Throws on error.
generateAndValidate
    :: (TLSConfiguration, DirConfiguration)
    -> IO (Property)
generateAndValidate (tlsConf, dirConf) = do
    (caDesc, certDescs) <-
        fromConfiguration tlsConf dirConf genRSA256KeyPair =<< genRSA256KeyPair

    (_, caCert) <- genCertificate caDesc

    let serverId = (NonEmpty.head $ serverAltNames $ tlsServer tlsConf, "")

    fmap conjoin $ forM certDescs $ \desc -> do
        (_, cert) <- genCertificate desc

        errors <-
            validateSHA256 caCert (certChecks desc) serverId cert

        return (errors === [])
