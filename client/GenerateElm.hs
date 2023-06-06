{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Servant.Elm                    ( DefineElm(DefineElm)
                                                , Proxy(Proxy)
                                                , defElmImports
                                                , defElmOptions
                                                , generateElmModuleWith
                                                , UrlPrefix(Static)
                                                , urlPrefix
                                                )
import           Api

main :: IO ()
main =
  generateElmModuleWith
    (defElmOptions {urlPrefix = Static "http://127.0.0.1:3000" })
    ["Api"]
    defElmImports
    "client/src"
    [DefineElm (Proxy :: Proxy Item), DefineElm (Proxy :: Proxy ItemId)]
    (Proxy :: Proxy Api)
