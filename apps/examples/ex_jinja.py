from forthic.interpreter import Interpreter
from forthic.modules.jinja_module import JinjaModule


def get_interp():
    interp = Interpreter()
    interp.register_module(JinjaModule)
    interp.run("['jinja'] USE-MODULES")
    return interp


def main():
    interp = get_interp()
    interp.dev_mode = True
    interp.run(
        """
    "Hello {{ name }}!" [['name' 'Obi Wan']] REC jinja.RENDER .s
    ["items"] VARIABLES
    [
        [ [["name" "Alpha"]] REC
          [["name" "Beta"]] REC
          [["name" "Gamma"]] REC ] items !
        '''
        {% for item in items %}
            * {{ item.name }}
        {%- endfor %}
        ''' [['items' items @]] jinja.RENDER .s
    """
    )


if __name__ == '__main__':
    main()
