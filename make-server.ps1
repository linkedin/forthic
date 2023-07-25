./myenv/Scripts/Activate.ps1
$env:FLASK_APP="run.py"
$env:FLASK_DEBUG="true"
cd server && flask run --port=5000