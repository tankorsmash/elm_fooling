# pydoit handles imports


def task_elm_make():
    """runs elm-make"""

    elm_make = ["elm", "make", "./src/Main.elm", "--output=main.js"]
    return {"actions": [elm_make]}


def task_copy():
    """assuming main.js was updated, copy the files to ../gh_site"""

    cp_js = ["cp", "main.js", "../gh_site/main.js"]
    cp_html = ["cp", "index.html", "../gh_site/index.html"]

    return {"actions": [cp_js, cp_html], "file_dep": ["main.js"]}


def task_update_site():
    """updates the site via git commit"""

    gcam = ["git", "-C", "../gh_site", "commit", "-am", "'update latest from CLI'"]
    gpush = ["git", "-C", "../gh_site", "push"]
    return {
        "actions": [gcam, gpush],
        "file_dep": ["../gh_site/main.js", "../gh_site/index.html"],
    }
