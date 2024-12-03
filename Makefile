##
## EPITECH PROJECT, 2022
## Makefile
## File description:
## makefile
##

NAME	=	glados

NAMECABAL	=	glados.cabal

PATHBIN =	$(shell stack path --local-install-root)

STACK	=	stack

all:	$(NAME)

$(NAME):
	$(STACK) build
	cp $(PATHBIN)/bin/glados-exe ./$(NAME)

clean:
	$(STACK) clean
	rm -f $(NAME)
	rm -f $(NAMECABAL)

fclean:	clean

re:	fclean all

docs:
	$(STACK) haddock

.PHONY:	re all clean fclean	docs
