{-# LANGUAGE LambdaCase #-}

module Home where

-- loadingScreen ::
loadingScreen =
  elClass "div" "mkOverlay" $ do
  iFa "fas fa-spinner fa-spin"
  text " Loading ..."

elFileInput
  :: DomBuilder t m
  => m (Event t File)
elFileInput = do
  i <- inputElement $ def
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes
         .~ ("type" =: "file" <> "accept" =: "text/cfg")

  let eFiles = _inputElement_files i
  pure $ fmapMaybe listToMaybe $ updated eFiles

frontendHome = do
    dyn_ $ ffor (stMsg <$> dynState) $ \case
      Nothing  -> blank
      Just str -> divMsgOverlay $ el "span" $ text str
    subRoute_ $ \case
      FrontendRoute_Main -> pageKeyboard

settings = el "div" $ do
    el "h3" $ text "Upload Plover config"

    eFile <- elFileInput

    eReqResult <- postRender $ do
      fileReader <- liftJSM newFileReader
      let encoding = Just ("utf8" :: String)
      performEvent_ $ fmap (\f -> readAsText fileReader (Just f) encoding) eFile
      eText <- fmap catMaybes $ wrapDomEvent fileReader (`on` load) $ liftJSM $ do
        v <- getResult fileReader
        (fromJSVal <=< toJSVal) v
      dynEitherText <- holdDyn (Left "no file") (Right . Text.unpack <$> eText)
      postConfigNew dynEitherText (void eText)

    widgetHold_ blank $ ffor eReqResult $ \case
      ResponseSuccess _ cfg _ -> elStageKeyboard cfg
      ResponseFailure _ msg _ -> el "span" $ text msg
      RequestFailure _ msg -> el "span" $ text msg
