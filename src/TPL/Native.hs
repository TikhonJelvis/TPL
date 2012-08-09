module TPL.Native where

import TPL.Error
import TPL.Value

curryOpr :: NativeOpr -> Term -> Result Value
curryOpr (NativeOpr name n fn) arg = return . Native . NativeOpr name (n - 1) $ \ args -> fn (arg:args)