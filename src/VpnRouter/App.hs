module VpnRouter.App where

import VpnRouter.Prelude ( ($), String, MonadIO, HasCallStack )
import UnliftIO.Exception ( stringException, throwIO )

type NetM m = (HasCallStack, MonadIO m)

ex :: NetM m => String -> m a
ex em = throwIO $ stringException em
