@echo off

ca65 sokoban.asm -o sokoban.o --debug-info
ld65 sokoban.o -o sokoban.nes -t nes --dbgfile sokoban.dbgfile
