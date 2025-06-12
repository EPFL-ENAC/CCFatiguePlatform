from fastapi import APIRouter, UploadFile, File, HTTPException
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