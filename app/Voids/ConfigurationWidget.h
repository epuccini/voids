#ifndef CONFIGURATIONWIDGET_H
#define CONFIGURATIONWIDGET_H

#include <QWidget>
#include <qttreepropertybrowser.h>
#include "ui_configuration.h"
#include "LispSettings.h"


class ConfigurationWidget : public QWidget
{
    Q_OBJECT
public:
    explicit ConfigurationWidget(QWidget *parent = 0);
    ~ConfigurationWidget();

    void populateTreeWidget(bool bInsertValues = true);
    void setConfigurationFile(QString file);
    void save(void);
    QString filePath(void);

    void showEvent(QShowEvent *event);

signals:

public slots:
    void slotPressClose(void);
    void slotPressNew(void);
    void slotPressLoad(void);
    void slotPressSave(void);
    void slotPressSaveAs(void);

private:
        Ui::FormConfiguration ui;
        bool isInitialized;
        QString configurationFile;
        LispSettings* settings;
        QtTreePropertyBrowser * browser;
};

#endif // CONFIGURATIONWIDGET_H
