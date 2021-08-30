import bottle
from bottle import route, run

from pathlib import Path

app = bottle.default_app()

_root_static_asset_dir_from_config = r"C:\Users\Josh\Documents\cocos_projects\magnolia_cocos\Resources\static_asset_dir"
root_static_asset_dir = Path(_root_static_asset_dir_from_config)

# app.config["TEST"] = "FOO"

@route("/hello")
def hello():
    return app.config

run(host="localhost", port=4126, debug=False, reloader=True)
