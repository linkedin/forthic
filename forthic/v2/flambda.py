"""
Lambdas are obiects that capture a sequence of word executions. A string can be thought of as a lambda if it
contains Forthic code. We can formalize this by defining a Lambda object that takes a Forthic string. To
execute this Lambda, we simply interpret the Forthic string.

Once all deferred code executions are viewed as Lambdas, we can manipulate them consistently. For instance, a
functor that mapped lambdas into lambdas that timed execution could convert any execution into a timed
execution. Similarly, a functor that wrapped a lambda in a try/catch block could keep track of errors during
execution.

If we insist that functors are isomorphisms, we can map any lambda back into a Forthic string. Then we could
transmit lambdas across Forthic interpreter boundaries and execute them in different contexts seamlessly.

To implement this properly, we need to replace every case that runs a string with a function that can run a
lambda (or a string).

Another benefit of this is being able to implement lambdas directly in the host language.
"""

from .interfaces import IInterpreter
from .module import Variable


class LambdaError(RuntimeError):
    pass


class ListExpectedError(LambdaError):
    pass


class Lambda:
    def __init__(self, forthic: str):
        self.forthic = forthic

    def execute(self, interp: IInterpreter):
        interp.run(self.forthic)


class NullOnErrorLambda(Lambda):
    """
    If an exception occurs while running a Lambda, return NULL and re-raise
    """
    def __init__(self, flambda: Lambda):
        self.flambda = flambda

    def execute(self, interp: IInterpreter):
        try:
            self.flambda.execute(interp)
        except Exception:
            interp.stack_push(None)
            raise


class AccumErrorLambda(Lambda):
    """
    Executes a Lambda in a try/except block and accumulates errors in an error variable
    """
    def __init__(self, flambda: Lambda, errors_var: Variable):
        self.flambda = flambda

        error_val = errors_var.get_value()
        if not isinstance(error_val, list):
            raise ListExpectedError(f"Expected variable to contain list not: {error_val}")
        self.errors_var = errors_var

    def execute(self, interp: IInterpreter):
        errors = self.errors_var.get_value()
        try:
            self.flambda.execute(interp)
            errors.append(None)
        except Exception as e:
            errors.append(e)
            self.errors_var.set_value(errors)
