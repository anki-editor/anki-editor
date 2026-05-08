import itertools
import json
from http.server import BaseHTTPRequestHandler, HTTPServer

import sys
import logging


class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
    """
    Mimics AnkiConnect server for testing purposes.
    """

    _next_note_id = itertools.count(999)
    _next_deck_id = itertools.count(9999)
    _next_card_id = itertools.count(99999)

    _recorded_requests = []
    _notes_info_tags = {}  # noteId (int) -> [tag str]

    def do_POST(self):
        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length)

        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()

        parsed_json = json.loads(post_data.decode('utf-8'))
        if not parsed_json.get('action', '').startswith('__test_'):
            self._recorded_requests.append(parsed_json)
        response_data = self.handle_action(parsed_json['action'], parsed_json.get('params'))
        response_message = json.dumps(response_data)

        self.wfile.write(response_message.encode('utf-8'))

    def handle_action(self, action, params=None):
        if action == 'modelNames':
            return {'result': ['Basic', 'Cloze', 'Basic (and reversed card)'], 'error': None}
        elif action == 'modelFieldNames':
            model_name = params['modelName']
            return {'result': self.action_model_field_names_response(model_name), 'error': None}
        elif action == 'storeMediaFile':
            file_name = params['filename']
            return {"result": file_name, "error": None}
        elif action == 'retrieveMediaFile':
            return {"result": "R0lGODlhAQABAIABAP///wAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==", "error": None}
        elif action == 'version':
            return {'result': 6, 'error': None}
        elif action == 'multi':
            actions = params['actions']
            result = [self.handle_action(item['action'], item.get('params')) for item in actions]
            return {'result': result, 'error': None}
        elif action == 'addNote':
            return {'result': next(self._next_note_id), 'error': None}
        elif action == 'createDeck':
            return {'result': next(self._next_deck_id), 'error': None}
        elif action == 'notesInfo':
            note_ids = params['notes']
            return {
                'result': [
                    {
                        'noteId': note_id,
                        'modelName': 'Basic',
                        'fieldOrder': 0,
                        'fields': {},
                        'tags': list(self._notes_info_tags.get(note_id, [])),
                        'cards': [next(self._next_card_id)],
                    }
                    for note_id in note_ids
                ],
                'error': None,
            }
        elif action == 'updateNote':
            return {'result': None, 'error': None}
        elif action == 'changeDeck':
            return {'result': None, 'error': None}
        elif action == '__test_reset__':
            type(self)._recorded_requests.clear()
            type(self)._notes_info_tags.clear()
            return {'result': None, 'error': None}
        elif action == '__test_get_requests__':
            return {'result': list(self._recorded_requests), 'error': None}
        elif action == '__test_set_notes_info_tags__':
            type(self)._notes_info_tags[params['noteId']] = list(params['tags'])
            return {'result': None, 'error': None}
        else:
            raise ValueError(f"Unknown action: {action}")

    def action_model_field_names_response(self, model_name):
        if model_name == 'Basic':
            return ['Front', 'Back']
        elif model_name == 'Cloze':
            return ['Text', 'Back Extra']
        elif model_name == 'Basic (and reversed card)':
            return ['Front', 'Back']
        else:
            raise ValueError(f"Unknown model name: {model_name}")


def run_server(host='localhost', port=28765):
    server_address = (host, port)
    httpd = HTTPServer(server_address, SimpleHTTPRequestHandler)
    print(f"Server running on http://{host}:{port}/")
    httpd.serve_forever()


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO, stream=sys.stdout)
    run_server()
