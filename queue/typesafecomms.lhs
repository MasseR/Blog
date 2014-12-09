Something that might be useful for my masters. Idea taken from Oliver Charles'
skillscast.

> {-# Language DataKinds #-}
> {-# Language TypeFamilies #-}
> {-# Language GADTs #-}

> data Command = GetOrder | GetProduct | InsertOrder

> data SCommand a where
>   SGetOrder :: SCommand 'GetOrder
>   SGetProduct :: SCommand 'GetProduct
>   SInsertOrder :: SCommand 'InsertOrder

> type InvoiceNumber = String
> type UPC = String
> data Order = Order deriving Read
> data Product = Product deriving Read
> data FindOrder = FindOrder InvoiceNumber
> data FindProduct = FindProduct UPC
> data NewOrder = NewOrder Order

> type family Request (c :: Command) :: * where
>   Request GetOrder = FindOrder
>   Request GetProduct = FindProduct
>   Request InsertOrder = NewOrder

> type family Response (c :: Command) :: * where
>   Response GetOrder = Maybe Order
>   Response GetProduct = Maybe Product
>   Response InsertOrder = (Either String ())

> withCommand :: Monad m => SCommand c -> (Request c -> m (Response c)) -> m ()
> withCommand SGetOrder action = do
>   _ <- action (FindOrder "")
>   return ()
> withCommand SGetProduct action = do
>   _ <- action (FindProduct "")
>   return ()
> withCommand SInsertOrder action = do
>   _ <- action (NewOrder Order)
>   return ()

> evalAction :: Monad m => String -> m ()
> evalAction "order.get" = withCommand SGetOrder (\(FindOrder _) -> return Nothing)
> evalAction "order.new" = withCommand SInsertOrder (\(NewOrder _) -> return (Left "Not implemented"))
> evalAction "product.get" = withCommand SGetProduct (\(FindProduct _) -> return Nothing)
> evalAction _ = fail "Unknown command"
