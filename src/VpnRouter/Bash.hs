module VpnRouter.Bash where

import VpnRouter.Prelude
    ( ($),
      pure,
      Monad((>>=)),
      Maybe(Just, Nothing),
      (<$>),
      trIo,
      printf,
      MonadIO(liftIO),
      ToString(toString),
      Text )
import VpnRouter.App ( ex, NetM )
import UnliftIO.Process ( callProcess )
import UnliftIO.Directory ( findExecutable )

checkAppOnPath :: NetM m => Text -> m ()
checkAppOnPath cmd =
  findExecutable (toString cmd) >>= \case
    Nothing -> ex $ printf "Tool [%s] is not on PATH" cmd
    Just _ -> pure ()

bash :: NetM m => Text -> [Text] -> m ()
bash cmd args = do
  liftIO $(trIo "/cmd args")
  callProcess (toString cmd) (toString <$> args)
