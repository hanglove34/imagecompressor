##
## EPITECH PROJECT, 2018
## MAKEFILE
## File description:
## Makefile
##

SRC	=   app/Main.hs

NAME	=	imageCompressor

all:	$(NAME)

$(NAME):	$(SRC)
		stack build --copy-bins --local-bin-path .
		mv imageCompressor-exe $(NAME)

clean:
	rm -rf .stack-work debruijn.cabal
	rm -rf $(NAME)

fclean: clean
	$(RM) $(NAME)

re: fclean all
