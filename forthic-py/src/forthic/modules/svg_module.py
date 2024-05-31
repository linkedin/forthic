from ..module import Module
from ..interfaces import IInterpreter


FORTHIC = ""


class SvgError(RuntimeError):
    pass


class SvgModule(Module):
    """This implements construction of SVG elements
    """

    def __init__(self, interp: IInterpreter):
        super().__init__('svg', interp, FORTHIC)

        # These are set by "flag words" to change the behavior of the words in this module
        self.flags = {
        }

        self.add_module_word('SVG>IMG-SRC', self.word_SVG_to_IMG_SRC)

        # Chart support
        self.add_module_word('AXIS', self.word_AXIS)
        self.add_module_word('VAL>PIX', self.word_VAL_to_PIX)
        self.add_module_word('TICK-VALUES', self.word_TICK_VALUES)

    # ( svg -- img_src )
    def word_SVG_to_IMG_SRC(self, interp: IInterpreter):
        svg = interp.stack_pop()
        result = f"data:image/svg+xml;utf8,{escape_svg(svg)}"
        interp.stack_push(result)

    # ( pix_y_0 pix_y_max y_values num_ticks -- Axis )
    def word_AXIS(self, interp: IInterpreter):
        num_ticks = interp.stack_pop()
        y_values = interp.stack_pop()
        pix_y_max = interp.stack_pop()
        pix_y_0 = interp.stack_pop()
        result = Axis(pix_y_0, pix_y_max, y_values, num_ticks)
        interp.stack_push(result)

    # ( Axis val -- pix )
    def word_VAL_to_PIX(self, interp: IInterpreter):
        val = interp.stack_pop()
        axis = interp.stack_pop()
        result = axis.val_to_pix(val)
        interp.stack_push(result)

    # ( Axis -- tick_values )
    def word_TICK_VALUES(self, interp: IInterpreter):
        axis = interp.stack_pop()
        result = axis.tick_values()
        interp.stack_push(result)


# ----- Helpers ----------------------------------------------------------------------------------------------
def escape_svg(string):
    result = string.replace('<', '%3C').replace('>', '%3E').replace('{', '%7B').replace('}', '%7D').replace('#', '%23').replace('"', '&quot;').replace("'", '&apos;')
    return result


class Axis:
    """This is a support class that figures out how to map data values to chart pixel values
    """
    def __init__(self, pix_0, pix_max, values, num_ticks):
        self.pix_0 = pix_0
        self.pix_max = pix_max
        self.values = values

        if num_ticks < 2:
            self.num_ticks = 2
        else:
            self.num_ticks = num_ticks

        if not values:
            raise SvgError("svg Axis: values not specified")

        self.val_0 = min(self.values)
        self.val_max = max(self.values) * 1.05

        self.tick_step = (self.val_max - self.val_0) / (self.num_ticks - 1)

        # Condition tick step when it's close to 5 or 10
        # TODO: Write this in a generic way
        if self.tick_step > 3 and self.tick_step < 5:
            self.tick_step = 5
        elif self.tick_step > 5 and self.tick_step < 10:
            self.tick_step = 10

        # Adjust val max based on tick step
        self.val_max = self.val_0 + self.tick_step * (self.num_ticks - 1)

        if self.val_max != self.val_0:
            self.pix_per_val = (self.pix_max - self.pix_0) / (self.val_max - self.val_0)
        else:
            self.pix_per_val = 1

    def val_to_pix(self, val):
        result = self.pix_0 + (val - self.val_0) * self.pix_per_val
        return result

    def tick_values(self):
        result = []
        for i in range(self.num_ticks):
            result.append(self.val_0 + self.tick_step * i)
        return result
