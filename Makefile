##
## EPITECH PROJECT, 2022
## Makefile
## File description:
## makefile
##

NAME	=	glados

NAMECABAL	=	glados.cabal

PATHBIN =	$(shell stack path --local-install-root)

all:	$(NAME)

$(NAME):
	stack build
	cp $(PATHBIN)/bin/glados-exe ./$(NAME)

test:
	stack test

retest:	re	test

clean:
	rm -f $(NAME)
	rm -f $(NAMECABAL)

fclean:	clean
	stack clean

re:	fclean all

.PHONY:	re all clean fclean test retest
