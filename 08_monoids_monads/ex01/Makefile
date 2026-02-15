NAME = main
SRC = main.ml

all: $(NAME)

$(NAME): $(SRC)
	ocamlc -o $(NAME) $(SRC)

clean:
	rm -f *.cmo *.cmi

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
