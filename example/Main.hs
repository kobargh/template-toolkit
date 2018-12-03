import Text.TemplateToolkit
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text as T

main = do
    cfg <- TIO.readFile "./conf.json"
    s <- evalTemplateFile "template.tt" (JSONstring cfg)
    case s of
        (Right txt) -> putStr . T.unpack $ txt
        (Left err) -> putStr ("ERROR! " ++ err)
        