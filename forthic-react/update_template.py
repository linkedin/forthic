import re
import glob

def main():
    # Get new js and css filenames
    [js_path] = glob.glob("./build/static/js/main*.js")
    [css_path] = glob.glob("./build/static/css/main*.css")
    js_file = re.match(".*(main\..*\.js)", js_path).group(1)
    css_file = re.match(".*(main\..*\.css)", css_path).group(1)

    # Load current template
    with open("../apps/examples/templates/react/react-app.html") as file:
        contents = file.read()

    # Replace current compiled files with new ones
    contents = re.sub("main\..*\.js", js_file, contents)
    contents = re.sub("main\..*\.css", css_file, contents)

    # Update file
    with open("../apps/examples/templates/react/react-app.html", "w") as file:
        file.write(contents)
    return

if __name__ == "__main__":
    main()