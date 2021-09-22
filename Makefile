SRCS				= 	srcs/ft_turing.hs \

OBJS1				= ${SRCS:.hs=.o}
OBJS2				= ${SRCS:.hs=.hi}

NAME				= ft_turing

FLAGS				= -Wall -Wextra -Werror

all :				
					ghc -o ${NAME} ${SRCS}

clean :				
					rm -rf ${OBJS1} ${OBJS2}

fclean : 			clean
					rm -rf ${NAME}

re :				fclean all

.PHONY:				all clean fclean re