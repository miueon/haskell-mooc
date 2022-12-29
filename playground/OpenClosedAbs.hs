-- data Vehicle = Car String | Airplane String
-- sound :: Vehicle -> String
-- sound (Car _) = "brum brum"
-- sound (Airplane _) = "zoooom"
{-# LANGUAGE InstanceSigs #-}

data Car = Car String

data Airplane = Airplane String

class VehicleClass a where
  sound :: a -> String

instance VehicleClass Car where
  sound :: Car -> String
  sound (Car _) = "fuck fuck"

data Discount
  = DiscountPercent Int -- A percentage discount
  | DiscountConstant Int -- A constant discount
  | MinimumPrice Int -- Set a minimum price
  | ForCustomer String Discount -- Discounts can be conditional
  | Many [Discount] -- Apply a number of discounts in row

