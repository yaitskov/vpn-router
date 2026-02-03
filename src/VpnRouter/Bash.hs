module VpnRouter.Bash where

import VpnRouter.Prelude
    ( Monad((>>)),
      (<$>),
      MonadIO(liftIO),
      ToString(toString),
      Text,
      trIo )
import VpnRouter.App ( NetM )
import UnliftIO.Process ( callProcess )


bash :: NetM m => Text -> [Text] -> m()
bash cmd args = liftIO $(trIo "/cmd args") >> callProcess (toString cmd) (toString <$> args)
