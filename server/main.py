import os
import sys
import json

import bottle
from bottle import route, run, get, post, response, request

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



@bottle.route('/<:re:.*>', method='OPTIONS')
def enable_cors_generic_route():
    """
    This route takes priority over all others. So any request with an OPTIONS
    method will be handled by this function.

    See: https://github.com/bottlepy/bottle/issues/402

    NOTE: This means we won't 404 any invalid path that is an OPTIONS request.
    """
    add_cors_headers()

@bottle.hook('after_request')
def enable_cors_after_request_hook():
    """
    This executes after every route. We use it to attach CORS headers when
    applicable.
    """
    add_cors_headers()

def add_cors_headers():
	bottle.response.headers['Access-Control-Allow-Origin'] = '*'
	bottle.response.headers['Access-Control-Allow-Methods'] = \
		'GET, POST, PUT, OPTIONS'
	bottle.response.headers['Access-Control-Allow-Headers'] = \
		'Origin, Accept, Content-Type, X-Requested-With, X-CSRF-Token'


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

class SetDirectory(object):
	"""Sets the cwd within the context

	  Args:
		  path (Path): The path to the cwd
	"""
	def __init__(self, path: Path):
		self.path = path
		self.origin = Path().absolute()

	def __enter__(self):
		os.chdir(self.path)

	def __exit__(self, *exc):
		os.chdir(self.origin)

@post("/torrent/search")
def torrent_search():
	# with SetDirectory(r"C:/code/python/qbitorrent"):
	# print("curdir", Path(os.curdir).absolute())
	sys.path.append(r"C:/code/python/qbitorrent")
	import downloader
	print("\njson:", request.json, "\n")

	query = request.json["query"]
	category = request.json["category"]
	episode = request.json.get("episode")
	season = request.json.get("season")
	allow_untrusted_users = request.json.get("allow_untrusted_users", False)

	(new_query, new_category) = downloader.build_query(
		query, category=category, episode=episode, season=season)
	# return { "success": True, "response": {"query": new_query, "category":new_category}}

	results = downloader.search(new_query, new_category)
	items = results.get('items', [])
	if not allow_untrusted_users:
		items = downloader.filter_by_trusted_users(items)

	return { "success": True, "response": {"items": items}}

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
