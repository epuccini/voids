#include <QMessageBox>
#include <QTextStream>
#include "LispSettings.h"

LispSettings::LispSettings(QWidget *parent, QString file)
{
    this->parent = parent;
    settingsFile = new QFile(file);
    settingsFile->open(QFile::ReadOnly | QFile::OpenModeFlag::Text);

    QTextStream in(settingsFile);

    while(!in.atEnd())
    {
        try
        {
            // group
            int delta = 0;
            QString line = in.readLine();
            if (!line.isEmpty())
            {
                // key
                if (line.at(line.length()-1) == " ")
                {
                    line = line.mid(0, line.length() - 1);
                }
                else
                {
                    line = line.mid(0, line.length());
                }
                if (line.at(line.length()-1) == ")")
                {
                    line = line.mid(0, line.length() - 1);
                }
                groupsList.push_back(line);
                content.append(line);

                int keyStart = 1;
                keyStart = line.indexOf(":", keyStart) + 1;
                int keyEnd = keyStart + 1;
                keyEnd = line.indexOf(" ", keyEnd);
                QString key = line.mid(keyStart, keyEnd - keyStart);
                keyList.push_back(key);

                // value
                int valueStart = keyEnd;
                valueStart = line.indexOf(" ", valueStart) + 1;
                if (line.at(valueStart) == "\"")
                 {
                    valueStart++;
                    delta = 1;
                }
                int valueEnd = valueStart + 1;
                valueEnd = line.indexOf(" ", valueEnd);

                // Space end of line
                if (line.indexOf(" ", valueStart + 1)  < 0)
                {
                    delta = -1;
                    valueEnd = line.length()-1;
                }
                //string
                if (line.at(valueEnd) == "\"")
                {
                    delta++;
                    valueEnd-= delta;
                }
                QString value = line.mid(valueStart, valueEnd - valueStart - delta);
                valueList.push_back(value);
            }
        }
        catch(...)
        {
            QMessageBox::question(
                parent, "Voids",
                QObject::tr("Error parsing LispSettings!"),
                QMessageBox::Ok,
                QMessageBox::Ok);
        }
    }; // end while
}

LispSettings::~LispSettings()
{
    delete settingsFile;
}

QString LispSettings::key(int idx)
{
    return keyList.at(idx);
}

QString LispSettings::value(int idx)
{
    return valueList.at(idx);
}

QString LispSettings::value(QString findKey)
{
    int idx = 0;
    QString value;

    for (QString key : keyList)
    {
        if (key == findKey)
        {
            value = valueList.at(idx);
            return value;
        }
        idx++;
    }
    return "";
}

QString LispSettings::toInitFile(void)
{
   if (!keyList.empty() && !valueList.empty())
   {
       QListIterator<QString> itrKeys (keyList);
       QListIterator<QString> itrValues (valueList);

       while (itrKeys.hasNext() && itrValues.hasNext())
       {
           QString key = itrKeys.next();
           QString value = itrValues.next();

           contentIniFormat.append(key + " = " + value + "\n");
       };
   }
   return contentIniFormat;
}
