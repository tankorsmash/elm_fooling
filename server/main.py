import json

import bottle
from bottle import route, run

from pathlib import Path

app = bottle.default_app()

_root_static_asset_dir_from_config = (
    r"C:\Users\Josh\Documents\cocos_projects\magnolia_cocos\Resources\static_asset_dir"
)
root_static_asset_dir = Path(_root_static_asset_dir_from_config)


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
