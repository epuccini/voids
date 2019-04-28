#ifndef LISPSETTINGS_H
#define LISPSETTINGS_H

#include <QString>
#include <QFile>

class LispSettings
{
public:
    LispSettings(QWidget *parent, QString file);
    ~LispSettings();

    QStringList childGroups(void) {
        return groupsList;
    }
    QStringList childKeys(void) {
        return keyList;
    }
    QStringList childValues(void) {
        return valueList;
    }

    QString key(int idx);
    QString value(int idx);
    QString value(QString key);

    QString toInitFile(void);

private:
    QString contentIniFormat;
    QWidget *parent;
    QString content;
    QFile* settingsFile;
    QStringList groupsList;
    QStringList keyList;
    QStringList valueList;
};

#endif // LISPSETTINGS_H
