module Rowbstones where

data DirR = Oeste | Este

data ExpR a = Lit a
              | PuedeMover DirR
              | NroBolitas
              | HayBolitas
              | UnOp UOp (ExpR a)
              | BinOp BOp (ExpR a) (ExpR a)

data UOp = No | Siguiente | Previo

data BOp = YTambien
         | OBien 
         | Mas 
         | Por

type ProgramaR = ComandoR

data ComandoR = Mover DirR
              | Poner
              | Sacar
              | NoOp
              | Repetir (ExpR Int) ComandoR
              | Mientras (ExpR Bool) ComandoR
              | Secuencia ComandoR ComandoR
