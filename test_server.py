import json
from http.server import BaseHTTPRequestHandler, HTTPServer


class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
    """
    Mimics AnkiConnect server for testing purposes.
    """

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
            return ['Basic', 'Cloze']
        elif action == 'modelFieldNames':
            model_name = params['modelName']
            return self.action_model_field_names_response(model_name)
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
        else:
            raise ValueError(f"Unknown action: {action}")

    def action_model_field_names_response(self, model_name):
        if model_name == 'Basic':
            return ['Front', 'Back']
        elif model_name == 'Cloze':
            return ['Text', 'Back Extra']
        else:
            raise ValueError(f"Unknown model name: {model_name}")


def run_server(host='localhost', port=28765):
    server_address = (host, port)
    httpd = HTTPServer(server_address, SimpleHTTPRequestHandler)
    print(f"Server running on http://{host}:{port}/")
    httpd.serve_forever()


run_server()
