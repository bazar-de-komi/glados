##
## EPITECH PROJECT, 2022
## Makefile
## File description:
## makefile
##

NAME		=	glados

NAMECABAL	=	glados.cabal

NAMETIX		=	glados-test.tix

PATHBIN 	=	$(shell stack path --local-install-root)

STACK		=	stack

all:	$(NAME)

$(NAME):
	$(STACK) build
	@cp $(PATHBIN)/bin/glados-exe ./$(NAME)

test:	clean
	stack test

retest:	re	test

clean:
	@rm -f $(NAME)
	@rm -f $(NAMECABAL)
	@rm -f $(NAMETIX)

fclean:
	$(STACK) clean
	$(MAKE) clean

re:	fclean	all

docs:
	$(STACK) haddock

.PHONY:	re	all	clean	fclean	docs	test	retest
