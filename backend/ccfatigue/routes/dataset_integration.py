from fastapi import APIRouter, UploadFile, File, HTTPException, BackgroundTasks
from fastapi.responses import FileResponse
from pathlib import Path
import shutil
import zipfile
import smtplib
from email.message import EmailMessage
from ccfatigue.config import settings
import tempfile
from typing import Optional

router = APIRouter()

def send_email_with_attachment(
    subject: str,
    body: str,
    file_path: Path,
    display_filename: Optional[str] = None
):
    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = settings.mail_from
    msg["To"] = settings.recipient_email
    msg.set_content(body)

    attach_name = Path(display_filename or file_path.name).name

    with open(file_path, "rb") as f:
        file_data = f.read()
        msg.add_attachment(
            file_data,
            maintype="application",
            subtype="zip",
            filename=attach_name
        )

    try:
        with smtplib.SMTP(str(settings.mail_server), str(settings.mail_port)) as smtp:  # type: ignore
            smtp.set_debuglevel(1)
            smtp.ehlo()
            if settings.mail_tls:
                smtp.starttls()
            smtp.login(str(settings.mail_username), str(settings.mail_password))
            smtp.send_message(msg)
        print("Email sent successfully!")
    except Exception as e:
        print(f"Error: {e}")
        raise HTTPException(status_code=500, detail=f"Error sending email: {str(e)}")



@router.post("/experiments/integrate_dataset")
async def integrate_dataset(file: UploadFile = File(...)):
    with tempfile.NamedTemporaryFile(delete=False, suffix=".zip") as tmp:
        shutil.copyfileobj(file.file, tmp)
        temp_zip_path = Path(tmp.name)

    try:
        send_email_with_attachment(
            subject="Dataset attached",
            body=(
                f"The dataset '{file.filename}' is attached to this email. "
                "The dataset must be unzipped and verified. Then, it can be added to the raw folder and preprocessed."
            ),
            file_path=temp_zip_path, 
            display_filename=file.filename
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error sending email: {str(e)}")
    finally:
        temp_zip_path.unlink(missing_ok=True)

    return {"success": True}

@router.get("/downloads_zip")
def download_zip(folder_name: str, background_tasks: BackgroundTasks):
    base_folder = Path(__file__).resolve().parents[3] / "frontend" / "public" / "downloads"
    folder_path = base_folder / folder_name

    if not folder_path.exists() or not folder_path.is_dir():
        raise HTTPException(status_code=404, detail="Folder not found")

    contents = list(folder_path.iterdir())
    if not contents:
        raise HTTPException(status_code=400, detail=f"The folder '{folder_name}' is empty and cannot be zipped.")

    zip_output_base = Path(tempfile.gettempdir()) / folder_name
    zip_path = shutil.make_archive(str(zip_output_base), 'zip', folder_path)

    background_tasks.add_task(Path(zip_path).unlink)

    return FileResponse(
        path=zip_path,
        filename=f"{folder_name}.zip",
        media_type="application/zip"
    )