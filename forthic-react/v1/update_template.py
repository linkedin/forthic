import re
import glob

def main():
    template_path = "../../server/templates/react/react-app/v1/main.html"

    # Get new js and css filenames
    [js_path] = glob.glob("./build/static/js/main*.js")
    [css_path] = glob.glob("./build/static/css/main*.css")
    js_file = re.match(".*(main\..*\.js)", js_path).group(1)
    css_file = re.match(".*(main\..*\.css)", css_path).group(1)

    # Load current template
    with open(template_path) as file:
        contents = file.read()

    # Replace current compiled files with new ones
    contents = re.sub("main\..*\.js", js_file, contents)
    contents = re.sub("main\..*\.css", css_file, contents)

    # Update file
    with open(template_path, "w") as file:
        file.write(contents)
    return

if __name__ == "__main__":
    main()