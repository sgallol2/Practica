import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un articulo
data Articulo = Articulo {
    nombre :: String,
    categoria :: String
} deriving (Show, Read)

-- Función para registrar la categoria de un articulo al inventario
registrararticulo :: String -> String -> [Articulo] -> [Articulo]
registrararticulo nombreArticulo categoriaArticulo inventario =
    Articulo nombreArticulo categoriaArticulo : inventario

-- Función para buscar articulo por nombre
buscarArticulo :: String -> [Articulo] -> Maybe Articulo
buscarArticulo nombreArticulo inventario =
    find (\art -> nombreArticulo == nombre art) inventario

-- Función para guardar la información de los articulos en un archivo de texto
guardarinventario :: [Articulo] -> IO ()
guardarinventario inventario = do
    result <- try (withFile "inventario.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarArticulo inventario))) :: IO (Either IOException ())
    case result of
        Left ex -> putStrLn $ "Error al guardar el inventario: " ++ show ex
        Right _ -> putStrLn "Inventario guardado en el archivo inventario.txt."

-- Función para cargar la información de los articulos desde un archivo de texto
cargarInventario :: IO [Articulo]
cargarInventario = do
    result <- try (withFile "inventario.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido) :: IO (Either IOException String)
    case result of
        Left ex -> do
            putStrLn $ "Error al cargar el inventario: " ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            return (map leerArticulo lineas)
    where
        leerArticulo linea = read linea :: Articulo

-- Función para mostrar la información de un articulo como cadena de texto
mostrarArticulo :: Articulo -> String
mostrarArticulo (Articulo nombre categoria) =
    "Articulo {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\"}"

-- Función para listar los articulos en el inventario
listarArticulos :: [Articulo] -> IO ()
listarArticulos [] = putStrLn "No hay articulos en el inventario."
listarArticulos articulos = do
    putStrLn "Articulos en el inventario:"
    mapM_ (putStrLn . mostrarArticulo) articulos

-- Función para contar los articulos en el inventario
contarArticulos :: [Articulo] -> IO ()
contarArticulos [] = putStrLn "No hay articulos en el inventario."
contarArticulos articulos = do
    let categorias = groupBy (\a b -> categoria a == categoria b) $ sortOn categoria articulos
    mapM_ (\cat -> putStrLn $ "Categoria: " ++ (categoria $ head cat) ++ ", Cantidad: " ++ show (length cat)) categorias

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el inventario desde el archivo de texto
    inventario <- cargarInventario
    putStrLn "¡Bienvenido al Sistema de Gestión de Inventario!"

    -- Ciclo principal del programa
    cicloPrincipal inventario

-- Función para el ciclo principal del programa
cicloPrincipal :: [Articulo] -> IO ()
cicloPrincipal inventario = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de articulo"
    putStrLn "2. Buscar articulo por nombre"
    putStrLn "3. Listar todos los articulos"
    putStrLn "4. Mostrar cantidad de articulos por categoria"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del articulo:"
            nombreArticulo <- getLine
            putStrLn "Ingrese la categoria del articulo:"
            categoriaArticulo <- getLine
            let inventarioActualizado = registrararticulo nombreArticulo categoriaArticulo inventario
            putStrLn $ "Articulo con nombre " ++ nombreArticulo ++ " y categoria " ++ categoriaArticulo ++ " ingresado al inventario."
            guardarinventario inventarioActualizado
            cicloPrincipal inventarioActualizado

        "2" -> do
            putStrLn "Ingrese el nombre del articulo a buscar:"
            nombreArticulo <- getLine
            case buscarArticulo nombreArticulo inventario of
                Just articulo -> do
                    putStrLn $ "El articulo con nombre " ++ nombreArticulo ++ " se encuentra en el inventario."
                    putStrLn $ "Categoria: " ++ categoria articulo
                Nothing -> putStrLn "Articulo no encontrado en el inventario."
            cicloPrincipal inventario

        "3" -> do
            listarArticulos inventario
            cicloPrincipal inventario

        "4" -> do
            contarArticulos inventario
            cicloPrincipal inventario

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal inventario