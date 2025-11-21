import json
from http.server import BaseHTTPRequestHandler, HTTPServer

import sys
import logging


class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
    """
    Mimics AnkiConnect server for testing purposes.
    """

    # In-memory storage for notes
    notes_db = {}
    next_note_id = 1000

    def do_POST(self):
        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length)

        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()

        parsed_json = json.loads(post_data.decode('utf-8'))
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
        elif action == 'createDeck':
            # Simple deck creation - just return success
            return {'result': None, 'error': None}
        elif action == 'changeDeck':
            # Simple deck change - just return success
            return {'result': None, 'error': None}
        elif action == 'addNote':
            return self.action_add_note(params)
        elif action == 'updateNoteFields':
            return self.action_update_note_fields(params)
        elif action == 'updateNote':
            return self.action_update_note(params)
        elif action == 'notesInfo':
            return self.action_notes_info(params)
        elif action == 'multi':
            actions = params['actions']
            result = [self.handle_action(item['action'], item.get('params')) for item in actions]
            return {'result': result, 'error': None}
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

    def action_add_note(self, params):
        """Handle addNote action - creates a new note and returns its ID."""
        note = params['note']
        note_id = SimpleHTTPRequestHandler.next_note_id
        SimpleHTTPRequestHandler.next_note_id += 1

        # Store note in database
        SimpleHTTPRequestHandler.notes_db[note_id] = {
            'noteId': note_id,
            'modelName': note['modelName'],
            'fields': note['fields'],
            'tags': note.get('tags', []),
            'deckName': note['deckName']
        }

        return {'result': note_id, 'error': None}

    def action_update_note_fields(self, params):
        """Handle updateNoteFields action - updates existing note fields."""
        note_id = params['note']['id']
        fields = params['note']['fields']

        if note_id not in SimpleHTTPRequestHandler.notes_db:
            return {'result': None, 'error': f'Note with ID {note_id} not found'}

        # Update fields
        SimpleHTTPRequestHandler.notes_db[note_id]['fields'].update(fields)

        return {'result': None, 'error': None}

    def action_update_note(self, params):
        """Handle updateNote action - updates note fields, deck, and tags."""
        note = params['note']
        note_id = note['id']

        if note_id not in SimpleHTTPRequestHandler.notes_db:
            return {'result': None, 'error': f'Note with ID {note_id} not found'}

        # Update all note properties
        if 'fields' in note:
            SimpleHTTPRequestHandler.notes_db[note_id]['fields'].update(note['fields'])
        if 'deckName' in note:
            SimpleHTTPRequestHandler.notes_db[note_id]['deckName'] = note['deckName']
        if 'tags' in note:
            SimpleHTTPRequestHandler.notes_db[note_id]['tags'] = note['tags']

        return {'result': None, 'error': None}

    def action_notes_info(self, params):
        """Handle notesInfo action - returns information about notes."""
        note_ids = params['notes']
        result = []

        for note_id in note_ids:
            if note_id not in SimpleHTTPRequestHandler.notes_db:
                # Return error for missing note
                result.append({'error': f'Note {note_id} not found'})
                continue

            note = SimpleHTTPRequestHandler.notes_db[note_id]

            # Format fields in the structure expected by anki-editor
            # Each field should have 'value' and 'order'
            field_names = self.action_model_field_names_response(note['modelName'])
            formatted_fields = {}
            for idx, field_name in enumerate(field_names):
                formatted_fields[field_name] = {
                    'value': note['fields'].get(field_name, ''),
                    'order': idx
                }

            result.append({
                'noteId': note['noteId'],
                'modelName': note['modelName'],
                'fields': formatted_fields,
                'tags': note['tags'],
                'cards': []  # Empty cards list for simplicity
            })

        return {'result': result, 'error': None}


def run_server(host='localhost', port=28765):
    server_address = (host, port)
    httpd = HTTPServer(server_address, SimpleHTTPRequestHandler)
    print(f"Server running on http://{host}:{port}/")
    httpd.serve_forever()


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO, stream=sys.stdout)
    run_server()
