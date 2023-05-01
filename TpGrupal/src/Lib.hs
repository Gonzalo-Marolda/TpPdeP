{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}

--data Sustancia = Sencilla String String Int String |
--               Compuesto String [String] Int String deriving(Show)

--                         Nombre  Simbolo 
-- data Sustancia = Sustancia String [String] String Int Int String deriving(Show) 

data Sustancia = Sustancia String [String] Int String deriving(Show)

oxigeno :: Sustancia
oxigeno = Sustancia "Oxigeno" ["O"] 8 "No metal"

hidrogeno :: Sustancia
hidrogeno = Sustancia "Hidrogeno" ["H"] 1 "No metal"

agua :: Sustancia
agua = Sustancia "Agua" ["H","2","O"] 0 "No metal"

conduceBien :: Sustancia -> String -> Bool
conduceBien (Sustancia _ simboloQuimico _ grupo) criterio
    | criterio == "Calor" = grupo == "Metal" || (grupo == "Halogeno" && length simboloQuimico > 1)
    | criterio == "Electricidad" = grupo == "Metal" || (grupo == "Gas noble" && length simboloQuimico == 1)
    | otherwise = False
  
nombreDeUnion :: Sustancia -> String
nombreDeUnion(Sustancia nombre _ _ _) = generarNombreDeUnion nombre
  
generarNombreDeUnion :: String -> String
generarNombreDeUnion nombre
  | last nombre `elem` "AEIOUaeiou" = generarNombreDeUnion (take (length nombre - 1) nombre)
  | otherwise = nombre ++ "uro"
  