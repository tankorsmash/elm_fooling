import json

import bottle
from bottle import route, run, get, post, response

from pathlib import Path

app = bottle.default_app()

_root_static_asset_dir_from_config = (
    r"C:\Users\Josh\Documents\cocos_projects\magnolia_cocos\Resources\static_asset_dir"
)
root_static_asset_dir = Path(_root_static_asset_dir_from_config)


class EnableCors:
    """Enable CORS support for http reequests"""

    name = "enable_cors"
    api = 2

    def apply(self, fn, context):
        def _enable_cors(*args, **kwargs):
            # set CORS headers
            response.headers["Access-Control-Allow-Origin"] = "*"
            response.headers["Access-Control-Allow-Methods"] = "GET, POST, PUT, OPTIONS"
            response.headers[
                "Access-Control-Allow-Headers"
            ] = "Origin, Accept, Content-Type, X-Requested-With, X-CSRF-Token"

            if bottle.request.method != "OPTIONS":
                # actual request; reply with the actual response
                return fn(*args, **kwargs)

        return _enable_cors


app.install(EnableCors())


def open_json_file(path):
    combo_path = root_static_asset_dir / (path)
    print("reading {}, which exists?: {}".format(combo_path, combo_path.exists()))
    with combo_path.open("r") as f:
        result = json.load(f)
        return result
    return None


FRAME_TYPES_TO_FILENAME = {
    "weapon": "all_weapon_frames.json",
    "armor": "all_armor_frames.json",
    "zone": "all_zone_frames.json",
    "weapon_category": "all_weapon_category_frames.json",
    "attribute": "all_attribute_frames.json",
    "battle_text_struct": "all_battle_text_struct_frames.json",
}


def error(message="no set error message"):
    return {"success": False, "message": message}


@post("/test")
def test_post(data):
    return {"success": True, "response": data}


@get("/test")
def test_get():
    return {"success": True}


@route("/frames/<frame_type>")
def frames(frame_type):
    # return app.config
    frame_filename = FRAME_TYPES_TO_FILENAME.get(frame_type)
    if not frame_filename:
        return error(f"invalid frame type: '{frame_type}'")

    json_data = open_json_file(Path("all_weapon_frames.json"))
    if not json_data:
        return error()

    return {"success": True, "json_data": json_data}


run(host="localhost", port=4126, debug=False, reloader=True)