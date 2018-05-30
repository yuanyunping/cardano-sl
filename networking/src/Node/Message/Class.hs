{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes        #-}

module Node.Message.Class
    ( Serializable (..)
    , hoistSerializable

    , Message (..)
    , messageCode'

    , MessageCode
    ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import           Data.Word (Word16)
import qualified Formatting as F
import           Node.Message.Decoder (Decoder, hoistDecoder)

-- * Message name

type MessageCode = Word16

-- | Defines type with it's own `MessageCode`.
class Message m where
    -- | Uniquely identifies this type
    messageCode :: Proxy m -> MessageCode

    -- | Description of message, for debug purposes
    formatMessage :: m -> T.Text
    default formatMessage :: F.Buildable m => m -> T.Text
    formatMessage = F.sformat F.build

-- | As `messageName`, but accepts message itself, may be more convinient is most cases.
messageCode' :: Message m => m -> MessageCode
messageCode' = messageCode . proxyOf
  where
    proxyOf :: a -> Proxy a
    proxyOf _ = Proxy

-- |
-- Terms which define a way to (de)-serialize object @r@ with given packing type @p@.
-- It abstract incremental deserialization and can be build on top of `Bi`
-- instance.
data Serializable packingType m t
    = Serializable
        { pack   :: t -> m LBS.ByteString
        , unpack :: Decoder m t
        }

hoistSerializable
    :: forall packingType m n t . Functor n
    => (forall a. m a -> n a)
    -> Serializable packingType m t
    -> Serializable packingType n t
hoistSerializable f (Serializable packS unpackS)
    = Serializable (f . packS) (hoistDecoder f unpackS)
