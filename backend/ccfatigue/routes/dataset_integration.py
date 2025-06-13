from fastapi import APIRouter, UploadFile, File, HTTPException, BackgroundTasks
from fastapi.responses import FileResponse
from pathlib import Path
import shutil
import zipfile
import smtplib
from email.message import EmailMessage
from ccfatigue.config import settings

router = APIRouter()

# Function to send an email with an attachment
def send_email_with_attachment(subject: str, body: str, file_path: Path):
    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = settings.mail_from
    msg["To"] = settings.recipient_email
    msg.set_content(body)

    with open(file_path, "rb") as f:
        file_data = f.read()
        msg.add_attachment(file_data, maintype="application", subtype="zip", filename=file_path.name)

    try:
        with smtplib.SMTP(settings.mail_server, settings.mail_port) as smtp:
            smtp.set_debuglevel(1)
            smtp.ehlo()
            if settings.mail_tls:
                smtp.starttls()
            smtp.login(settings.mail_username, settings.mail_password)
            smtp.send_message(msg)
        print("Email sent successfully!")
    except Exception as e:
        print(f"Error: {e}")
        raise HTTPException(status_code=500, detail=f"Error sending email: {str(e)}")

# New endpoint to send the ZIP file as an email attachment
@router.post("/experiments/integrate_dataset")
async def integrate_dataset(file: UploadFile = File(...)):
    temp_zip_path = Path(f"TEMP_{file.filename}")
    with temp_zip_path.open("wb") as buffer:
        shutil.copyfileobj(file.file, buffer)

    try:
        send_email_with_attachment(
            subject="Dataset attached",
            body=(
            f"The dataset '{file.filename}' is attached to this email. "
            "The dataset must be unzipped, checked again and added to the raw folder and then preprocessed. "
            "Remember to remove the TEMP_ in front of the .zip filename."
            ),
            file_path=temp_zip_path
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error sending email: {str(e)}")
    finally:
        temp_zip_path.unlink()

    return {"success": True}

    return {"success": True}


def send_email_old(subject: str, body: str):
    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = settings.mail_from
    msg["To"] = settings.recipient_email
    msg.set_content(body)

    try:
        with smtplib.SMTP(settings.mail_server, settings.mail_port) as smtp:
            smtp.set_debuglevel(1)
            smtp.ehlo()
            if settings.mail_tls:
                smtp.starttls()
            smtp.login(settings.mail_username, settings.mail_password)
            smtp.send_message(msg)
        print("Email sent successfully!")
    except Exception as e:
        print(f"Error: {e}")
        raise HTTPException(status_code=500, detail=f"Error sending email: {str(e)}")

@router.post("/experiments/integrate_dataset_old")
async def integrate_dataset_old(file: UploadFile = File(...)):
    extract_path = Path("../Data/raw")
    extract_path.mkdir(parents=True, exist_ok=True)

    temp_zip_path = Path(f"temp_{file.filename}")
    with temp_zip_path.open("wb") as buffer:
        shutil.copyfileobj(file.file, buffer)

    try:
        with zipfile.ZipFile(temp_zip_path, "r") as zip_ref:
            zip_ref.extractall(extract_path)
    except zipfile.BadZipFile:
        raise HTTPException(status_code=400, detail="The file is not a valid ZIP archive.")
    finally:
        temp_zip_path.unlink()

    try:
        send_email_old(
            subject="Dataset successfully integrated",
            body=f"The dataset '{file.filename}' has been successfully integrated into '{extract_path}'."
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error sending email: {str(e)}")

    return {"success": True}

@router.get("/downloads_zip")
def download_zip(folder_name: str, background_tasks: BackgroundTasks):
    # Absolute path to the "frontend/public/downloads" directory
    base_folder = Path(__file__).resolve().parents[3] / "frontend" / "public" / "downloads"
    folder_path = base_folder / folder_name

    # Ensure the folder exists and is a directory
    if not folder_path.exists() or not folder_path.is_dir():
        raise HTTPException(status_code=404, detail="Folder not found")

    # Check that the folder is not empty
    contents = list(folder_path.iterdir())
    if not contents:
        raise HTTPException(status_code=400, detail=f"The folder '{folder_name}' is empty and cannot be zipped.")

    # Generate the ZIP archive in /tmp
    zip_output_base = Path(f"/tmp/{folder_name}")
    zip_path = shutil.make_archive(str(zip_output_base), 'zip', folder_path)

    # Schedule the ZIP file for deletion after the response is sent
    background_tasks.add_task(Path(zip_path).unlink)

    # Return the ZIP file as a response
    return FileResponse(
        path=zip_path,
        filename=f"{folder_name}.zip",
        media_type="application/zip"
    )