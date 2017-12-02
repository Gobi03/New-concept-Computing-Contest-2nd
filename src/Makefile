.PHONY: all clean submit
all:
	./all-compile.sh

submit:
	./make-submit.sh

test:
	scala Main < ../testcase.in > ../result.out
	../scripts/score_evaluator.out ../testcase.in ../result.out

clean:
	${RM} *.class
