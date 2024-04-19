--------LINK para compilar :: https://code.world/haskell#PGa_bOCk1hptL4VoF0ryjsQ -----


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import CodeWorld
import Data.Char
import System.Random



type Pixel = Char
type Pic   = [[Pixel]   ]
type Alien = (Point, Pic)
type ProjetilAlien = Point 
type Vida = (Point, Pic)
type ExplodeAlien = Point

main = do
       g <- newStdGen
       let naleatorios = randomRs (1, 10) g :: [Int]
       activityOf World  {
                           matrizAliens                  = (matrizAliens1, matrizAliens2),
                           posicaoMAliens                = (0, 0),
                           direcaoAlien                  = 1,
                           temporizadorAlien             = 0.5,
                           posicaoXNave                  = 0,
                           direcaoNave                   = 0,
                           posicaoBala                   = (0, -11),
                           velocidadeXBala               = 0,
                           movimentoBala                 = 0,
                           numerosAleatorios             = naleatorios,
                           temporizadorProjeteisAliens   = 0.35,
                           projeteisAliens               = [],
                           temporizadorExplosao          = 1.05,
                           imagensExplosao               = (explode1Pic, explode2Pic),
                           temporizadorMovimentoExplosao = 0.105,
                           fimDeJogo                     = 1,
                           estaExplodindo                = False,
                           vidasRestantes                = [((5, 9), navePic), ((7, 9), navePic), ((9, 9), navePic)],
                           alienMorto                    = [],
                           temporizadorMorteAlien        = 0.11,
                           aliensDesaparece              = False}
                       atualize vizualize


data World = World { 
                    direcaoAlien                  :: Double,
                    aliensDesaparece              :: Bool,
                    matrizAliens                  :: ([Alien], [Alien]),
                    posicaoXNave                  :: Double,
                    alienMorto                    :: [ExplodeAlien],
                    vidasRestantes                :: [Vida],
                    projeteisAliens               :: [ProjetilAlien],
                    temporizadorProjeteisAliens   :: Double,
                    posicaoBala                   :: Point,
                    imagensExplosao               :: (Pic, Pic),
                    temporizadorMorteAlien        :: Double,
                    temporizadorMovimentoExplosao :: Double,
                    posicaoMAliens                :: Point,
                    movimentoBala                 :: Double,
                    numerosAleatorios             :: [Int],
                    temporizadorAlien             :: Double,
                    direcaoNave                   :: Double,
                    temporizadorExplosao          :: Double,
                    fimDeJogo                     :: Double,
                    estaExplodindo                :: Bool,
                    velocidadeXBala               :: Double }
                     




------------------ Transformação de strings para Pictures ----------------------
             
stringDesenho :: Pic -> Picture
stringDesenho pic = translated ((-larguraPic / 2) + 0.5) ((alturaPic / 2) - 0.5) $
                    pictures $ renderizaImagem  pic
    where
        larguraPic = fromIntegral $ length $ head pic
        alturaPic  = fromIntegral $ length pic

renderizaImagem :: Pic -> [Picture]
renderizaImagem pic                     = concatMap renderizaLinha (zip [0..] pic)

renderizaLinha :: (Double, String) -> [Picture]
renderizaLinha (rowIndex, linha)        = concatMap (renderizaPixel rowIndex) (zip [0..] linha)

renderizaPixel :: Double -> (Double, Char) -> [Picture]
renderizaPixel rowIndex (colIndex, '#') = [translated colIndex (-rowIndex) (colored black (solidRectangle 1 1))]
renderizaPixel _ _                      = []
               
       
       



-----------------------funções vcizualização -----------------------------------




vizualize w@World {..} =
    vizualizetodosAliens w &
    vizualizeExplosaoDaNave w &
    vizualizeBala w &
    vizualizeProjetil w &
    vizualizeVidaNave w &
    vizualizeExplosaoDoAlien w &
    constelacao &  
    fundo 
   where 
     
     fundo       = colored black $ solidRectangle 30 30
     constelacao = pictures [ estrela (-2) 7, estrela 3 (-5), estrela (-7) (-8), estrela 2 8, estrela (-9) (-1)
                           , estrela (-4) 3, estrela 1 5, estrela 8 (-6), estrela 4 (-7), estrela (-4) (-4)
                           , estrela 5 8, estrela (-6) (-9), estrela 0 8, estrela 2 (-5), estrela (-3) 3
                           , estrela 4 (-3), estrela (-2) 5, estrela 1 (-1), estrela (-5) 7, estrela 3 (-5)
                           , estrela (-1) 9
                         ]
                where 
                    estrela x y = translated x y $ 
                                 (colored white (solidCircle 0.1) & rotated (pi / 4) (solidRectangle 0.2 0.03) & rotated (pi / 4) (solidRectangle 0.2 0.03))


vizualizetodosAliens World {..} =
    colored white     .
    translated kx ky  .
    dilated 1.5       .
    pictures          . 
    map transformaPic $
    matriz1
  where
    (matriz1, matriz2)          = matrizAliens
    (kx, ky)                    = posicaoMAliens
    transformaPic ((x, y), pic) = translated x y (dilated 0.06 (stringDesenho pic))
    
    

vizualizeExplosaoDaNave World {..}
    | fimDeJogo == 0 = colored red                      .
                       translated posicaoXNave posicaoYNave . 
                       dilated 0.15                     .
                       stringDesenho                    $
                       exp1
                       
    | otherwise      = colored red                      . 
                       translated posicaoXNave posicaoYNave . 
                       dilated 0.15                     .
                       stringDesenho                    $ 
                       navePic
  where 
    (exp1, exp2) = imagensExplosao



vizualizeBala World {posicaoBala = (bx, by)} = colored red      .
                                               translated bx by $ 
                                               solidRectangle 0.17 0.3

vizualizeProjetil World {..} = pictures .
                              map (\(x, y) -> colored white . translated x y . dilated 0.1 $ stringDesenho projetilPic) $
                              projeteisAliens

vizualizeVidaNave World {..} = pictures .
                               map (\((vx, vy), p) -> colored red . translated vx vy . dilated 0.1 $ stringDesenho p) 
                               $ vidasRestantes

vizualizeExplosaoDoAlien World {..} 
  | temporizadorMorteAlien <= 0 = blank
  | otherwise                   = colored red . pictures . map (\(kx, ky) -> translated kx (ky + 0.5) . dilated 0.1 $ stringDesenho morteAlienPic) $ alienMorto
       
                   
                   
                   
----------------------funções atualize -----------------------------------------
                    
atualize :: Event -> World -> World
atualize (TimePassing t) w      = atualizaTempo t w
atualize (KeyPress "Right") w   = w {direcaoNave  = 1 } 
atualize (KeyRelease "Right") w =  w {direcaoNave  = 0 }
atualize (KeyPress "Left") w    = w {direcaoNave  = -1 }
atualize (KeyRelease "Left") w  =   w {direcaoNave  = 0 }
atualize (KeyPress " ") w       = dispara  w
atualize _ w                    = w


atualizaTempo t w@World {..} = mortesAliens t    $ 
                               explodeNave t     $
                               projetilAliens t  $
                               atiraBala t       $
                               movimentaNave t   $
                               movimentaAliens t $
                               w
  
  
  
  
-----------------------funções para movimentação--------------------------------
   
   
   
movimentaAliens t w@World {..}
    | temporizadorAlien > 0  =
        w {temporizadorAlien = temporizadorAlien - t}
    | direcaoAlien == 1 && novoKX > 3.5 || direcaoAlien == -1 && novoKX < -3.5 =
        w {matrizAliens      = mudaAliens (matriz1, matriz2),
           posicaoMAliens    = (kx, novoKY),
           direcaoAlien      = -direcaoAlien,
           temporizadorAlien = 0.5}
    | otherwise              =
        w {matrizAliens      =
             if fimDeJogo == 0
             then matrizAliens
             else mudaAliens (matriz1, matriz2),
           posicaoMAliens    = (novoKX, ky),
           temporizadorAlien = 0.5}
    where
        (matriz1, matriz2)            = matrizAliens
        (kx, ky)                      = posicaoMAliens
        mudaAliens (matriz1, matriz2) = (matriz2, matriz1)
        novoKX                        = kx + direcaoAlien * deslAliens * fimDeJogo
        novoKY                        = ky - 0.5 * fimDeJogo

movimentaNave t w@World {..} =
    w {posicaoXNave          = posicaoXNave + direcaoNave * velXNave * t * fimDeJogo}
                                         


     
---------------- funções disparos ----------------------------------------------



atiraBala t w@World {..} =
    w {posicaoBala       = (bx + movimentoBala * velocidadeXBala * t, by + movimentoBala * velYBala * t)}
    where
        (bx, by)         = posicaoBala

dispara w@World {..}
    | dentroEspaço         =
        w {posicaoBala     = (posicaoXNave, posicaoYNave),
           velocidadeXBala =  direcaoNave * velXNave,
           movimentoBala   = 1}
    | otherwise =
        w {movimentoBala = 1}
    where 
        (px, py)     = posicaoBala
        dentroEspaço = px <= -10 || px >= 10 || py >= 10 || py <= -10

projetilAliens t w@World {..}
    | fimDeJogo == 0 =
        w
    | temporizadorProjeteisAliens <= 0 =
        w {temporizadorProjeteisAliens = novoTimer,
           projeteisAliens             = desceProj projeteisAtualizados,
           numerosAleatorios           = restoRnds}
    | otherwise                        =
        w {temporizadorProjeteisAliens = temporizadorProjeteisAliens - t,
           projeteisAliens             = desceProj projeteisAliens}
    where
        (ma1, ma2)                = matrizAliens
        rnd1 : rnd2 : restoRnds   = numerosAleatorios
        projeteisAtualizados
            | posAtiradores == [] = projeteisAliens
            | otherwise           = novoProjetil : projeteisAliens
        novoProjetil              = posAtiradores !! mod rnd2 (length posAtiradores)
        posAtiradores             = map fst $ take 11 posAlienAtual
        novoTimer                 = fromIntegral rnd1 * 0.3
        desceProj                 = map (\(px, py) -> (px, py + velYProj * t))
        posAlienAtual             = map (\((kx, ky), p) -> ((kx + pmx, ky + pmy), p)) ma1
        (pmx, pmy)                = posicaoMAliens



colisaoTiroAliens _ []                                        = False
colisaoTiroAliens posicaoBala ((posAlien, _) : restanteAlien) = 
         distancia posAlien posicaoBala <= 0.7 ||  colisaoTiroAliens posicaoBala restanteAlien



-------------------- funções  para explosões -----------------------------------


       
explodeNave t w@World {..}
    | projetilAtingeNave                 =
        w {projeteisAliens               = [],
           vidasRestantes                = tail vidasRestantes,
           fimDeJogo                     = 0,
           estaExplodindo                = True}
    | temporizadorExplosao < 0           =
        w {posicaoXNave                  = 0,
           temporizadorMovimentoExplosao = 1,
           temporizadorExplosao          = 0.1,
           fimDeJogo                     = 1,
           estaExplodindo                = False}
    | estaExplodindo                     =
        w {temporizadorExplosao          = temporizadorExplosao - t,
           imagensExplosao               = trocaImagens imagensExplosao,
           temporizadorMovimentoExplosao = temporizadorMovimentoExplosao - t}
    | vidasRestantes == []               =
        w {matrizAliens                  = (matrizAliens1, matrizAliens2),
           posicaoMAliens                = (0, 0),
           direcaoAlien                  = 1,
           posicaoXNave                  = 0,
           temporizadorExplosao          = 1,
           temporizadorMovimentoExplosao = 0.1,
           fimDeJogo                     = 1,
           estaExplodindo                = False,
           vidasRestantes                = [((4, 9), navePic), ((6, 9), navePic), ((8, 9), navePic)]}
    | otherwise                          = w
    where
        projetilAtingeNave        = any colide projeteisAliens
        colide (px, py)           = dist (px, py) (posicaoXNave, posicaoYNave) <= 0.5
        dist (x1, y1) (x2, y2)    = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
        trocaImagens (exp1, exp2) = (exp2, exp1)



mortesAliens t w@World {..}
    | teveColisao                 =
        w {matrizAliens           = atualizaMatrizAliens,
           posicaoBala            = (0, -11),
           movimentoBala          = 0,
           alienMorto             = (nmx, nmy) : alienMorto,
           aliensDesaparece       = True}
    | temporizadorMorteAlien <= 0 =
        w {alienMorto             = init alienMorto,
           temporizadorMorteAlien = 0.1,
           aliensDesaparece       = False}
    | aliensDesaparece            =
        w {temporizadorMorteAlien = temporizadorMorteAlien - t}
    | matrizAliens == ([], [])    =
        w {matrizAliens           = (matrizAliens1, matrizAliens2),
           posicaoMAliens         = (0, 0),
           direcaoAlien           = 1,
           posicaoXNave           = 0}
    | otherwise                   = w
    where
        teveColisao           = any colisao posAlienAtual
        colisao (posAlien, _) = distancia posAlien posicaoBala <= 0.5
        (ma1, ma2)            = matrizAliens
        atualizaMatrizAliens  = (filtraAlien ma1, filtraAlien ma2)
        filtraAlien           = filter (\((kx, ky), _) -> (kx, ky) /= (nmx - pmx, nmy - pmy))
        (nmx, nmy)            = posDoAlien $ map fst posAlienAtual
        posDoAlien [posAlien] = posAlien
        posDoAlien (posAlien : ps)
            | colisao (posAlien, posicaoBala) = posAlien
            | otherwise                       = posDoAlien ps
        posAlienAtual                         = map (\((kx, ky), p) -> ((kx + pmx, ky + pmy), p)) ma1
        (pmx, pmy)                            = posicaoMAliens



distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


--------------constantes -------------------------------------------------------
posicaoYNave   = -8
deslAliens = 0.2
velXNave   = 7
velYBala   = 8
velYProj   = -6



----------------matriz dos Aliens ----------------------------------------------


matrizAliens1 = zip coordenadas listaAliens1
matrizAliens2 = zip coordenadas listaAliens2

coordenadas  = pontos  ( (replicate 11 5) ++(replicate 11 4) ++(replicate 11 3) ++(replicate 11 2) ++ (replicate 11 1)   ) 
                                               
pontos =  zip (concat . replicate 6 $ [-5..5])                                         
                                         

listaAliens1 = (replicate 20 alien3_1) ++ (replicate 20 alien2_1) ++ (replicate 20 alien1_1)
listaAliens2 = (replicate 20 alien3_2) ++ (replicate 20 alien2_2) ++ (replicate 20 alien1_2)





------------------------------- imagens ---------------------------------------- 


  
alien1_1 = [ "     ####     ",
             " ############ ",
             "##############",
             "####  ##  ####",
             "##############",
             "   ###  ###   ",
             "  ##  ##  ##  ",
             "   ##    ##   " ]
             
alien1_2 = [ "     ####     ",
             " ############ ",
             "##############",
             "####  ##  ####",
             "##############",
             "   ###  ###   ",
             "  ##  ##  ##  ",
             "##          ##" ]
             
alien2_1 = [ "  #     #  ",
             "   #   #   ",
             "  #######  ",
             "### ### ###",
             "###########",
             "# ####### #",
             "# #     # #",
             "   ## ##   " ]
             
alien2_2 = [ "  #     #  ",
             "#  #   #  #",
             "# ####### #",
             "### ### ###",
             "###########",
             "  #######  ",
             "  #     #  ",
             " #       # " ]
             
alien3_1 = [ "   ##   ",
             "  ####  ",
             " ###### ",
             "## ## ##",
             "########",
             " # ## # ",
             "#      #",
             " #    # " ]
             
alien3_2 = [ "   ##   ",
             "  ####  ",
             " ###### ",
             "## ## ##",
             "########",
             "  #  #  ",
             " # ## # ",
             "# #  # #" ]
                     
navePic =  [ "        ##       ",
             "       #  #      ",
             "   #   #  #   #  ",
             "   #  ######  #  ",
             "   # # #### # #  ",
             "   ##   ##   ##  ",
             "  #  ########  # ",
             "  ############## "
           ]
                               
           
projetilPic = [ " # ",
                "###",
                "###",
                " # " ]
                
explode1Pic = [ "      #      # ",
                "         #  #  ",
                "  #   #        ",
                "       ## ##   ",
                "#    # ## # #  ",
                "  ##     ###  # ",
                "      ###### # " ]

explode2Pic = [ " #          #  ",
                "  #   ##  ##   ",
                "    #        # ",
                "#   ##  ##    #",
                " #   ###    #  ",
                "               ",
                " ## ######   # " ]
              
morteAlienPic = [ "   #   #   #      ",
                  "    #  #  #       ", 
                  "     # # #        ", 
                  "       #          ",
                  "###         ###   ",
                  "       #          ",
                  "     # # #        ",
                  "    #  #  #       ",
                  "   #   #   #      " ]
                  
planeta1 = ["    ##    ",
            "  ######  ",
            "##########",
            "##########",
            "  ######  ",
            "    ##    "]

planeta2 = ["  ######  ",
            "##########",
            "##########",
            "##########",
            "##########",
            "  ######  "]

planeta3 = ["    ######    ",
            "  ############  ",
            "################",
            "################",
            "################",
            "  ############  ",
            "    ######    "]                  
