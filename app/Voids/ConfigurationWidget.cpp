#include <QShowEvent>
#include <qtpropertymanager.h>
#include <qtpropertybrowser.h>
#include <qteditorfactory.h>
#include <QMessageBox>
#include <QFileDialog>

#include "ConfigurationWidget.h"

ConfigurationWidget::ConfigurationWidget(QWidget *parent) : QWidget(parent)
{
    ui.setupUi(this);
    settings  = nullptr;
    browser = nullptr;
    ui.pushButtonClose->setMaximumHeight(20);
    ui.pushButtonLoad->setMaximumHeight(20);
    ui.pushButtonSave->setMaximumHeight(20);
    ui.pushButtonSaveAs->setMaximumHeight(20);
    ui.pushButtonNew->setMaximumHeight(20);
    ui.pushButtonClose->setStyleSheet("background: #555555; color: #EEEEEE");
    ui.pushButtonLoad->setStyleSheet("background: #555555; color: #EEEEEE");
    ui.pushButtonSave->setStyleSheet("background: #555555; color: #EEEEEE");
    ui.pushButtonSaveAs->setStyleSheet("background: #555555; color: #EEEEEE");
    ui.pushButtonNew->setStyleSheet("background: #555555; color: #EEEEEE");
    ui.frameBrowser->setStyleSheet("outline-color: #444444; background-color: #222222; color: white; " \
                                   "selection-background-color: #CCCCCC; selection-color: #222222; " \
                                   "alternate-background-color: #555555");
}

ConfigurationWidget::~ConfigurationWidget()
{
    if (settings)
        delete settings;
}

void ConfigurationWidget::showEvent(QShowEvent *event)
{
    QWidget::showEvent(event);
    if (event->spontaneous())
        return;

    if (isInitialized)
        return;

    isInitialized = true;
}

void ConfigurationWidget::setConfigurationFile(QString file)
{
    this->configurationFile = file;
    QFile* testFile = new QFile(file);
    if (testFile->exists())
    {
        if (settings)
            delete settings;
        settings = new LispSettings(this, file);
        populateTreeWidget();
        delete settings;
        settings = nullptr;
    }
    else
    {
        this->close();
    }
}

void ConfigurationWidget::populateTreeWidget(bool bInsertValues)
{
    if (settings)
    {
        if (browser)
        {
            delete browser;
            browser = nullptr;
        }
        browser = new QtTreePropertyBrowser(ui.frameBrowser);
        browser->setMinimumWidth(ui.frameBrowser->width());
        browser->setMaximumWidth(ui.frameBrowser->width());
        browser->setMinimumHeight(ui.frameBrowser->height());
        browser->setMaximumHeight(ui.frameBrowser->height());
        browser->setHeaderVisible(false);

        for (QString key : settings->childKeys())
        {
            QString valueString = settings->value(key);
            QtStringPropertyManager* stringManager;
            QtDoublePropertyManager* doubleManager;
            QtBoolPropertyManager* boolManager;
            QtIntPropertyManager* intManager;
            QtEnumPropertyManager* enumManager;
            QtProperty* property = nullptr;

            if (key == "N-OUTPUT-TH" || key == "NN-LEARNING-RATE" || key == "NN-MOMENTUM-COEFF"
            || key == "NN-FINAL-TARGET-ERROR"|| key == "NN-ABORT-ERROR")
            {
                doubleManager = new QtDoublePropertyManager(this);
                property = doubleManager->addProperty(key);
                property->setValueToolTip(valueString);
                double valueDouble = valueString.replace("d0", "0").toDouble();
                if (bInsertValues)
                    doubleManager->setValue(property, valueDouble);
                QtDoubleSpinBoxFactory* doubleSpinBoxFactory = new QtDoubleSpinBoxFactory();
                browser->setFactoryForManager(doubleManager, doubleSpinBoxFactory);
            }
            else if (key == "GNUPLOT-PATH" || key == "IMAGEFILES-DIR" || key == "OUTFILES-DIR"
            || key == "INFILES-DIR"|| key == "MARSHAL-DATAFILE" || key == "PRG-NAME")
            {
                stringManager = new QtStringPropertyManager(this);
                property = stringManager->addProperty(key);
                property->setValueToolTip(valueString);
                if (bInsertValues)
                   stringManager->setValue(property, valueString);
                QtLineEditFactory* lineEditFactory = new QtLineEditFactory();
                browser->setFactoryForManager(stringManager, lineEditFactory);
            }
            else if (key == "TRAINING-INFILE" || key == "TRAINING-OUTFILE" || key == "TRAINING-ERRFILE"
            || key == "VALIDATION-INFILE" || key == "VALIDATION-OUTFILE"
            || key == "TRAINING-PLOT" || key == "VALIDATION-PLOT"|| key == "ERROR-PLOT")
            {
                stringManager = new QtStringPropertyManager(this);
                property = stringManager->addProperty(key);
                property->setValueToolTip(valueString);
                if (bInsertValues)
                    stringManager->setValue(property, valueString);
                QtLineEditFactory* lineEditFactory = new QtLineEditFactory();
                browser->setFactoryForManager(stringManager, lineEditFactory);
            }
            else if (key == "IMG-EXT")
            {
                QStringList actList;
                enumManager = new QtEnumPropertyManager(this);
                property = enumManager->addProperty(key);
                property->setValueToolTip(valueString);
                actList << ".png" << ".svg";
                enumManager->setEnumNames(property, actList);
                int enumValue = actList.indexOf(valueString);
                if (bInsertValues)
                    enumManager->setValue(property, enumValue);
                QtEnumEditorFactory* enumFactory = new QtEnumEditorFactory();
                browser->setFactoryForManager(enumManager, enumFactory);
            }
            else if (key == "DAT-EXT")
            {
                QStringList actList;
                enumManager = new QtEnumPropertyManager(this);
                property = enumManager->addProperty(key);
                property->setValueToolTip(valueString);
                actList << ".dat";
                enumManager->setEnumNames(property, actList);
                int enumValue = actList.indexOf(valueString);
                if (bInsertValues)
                    enumManager->setValue(property, enumValue);
                QtEnumEditorFactory* enumFactory = new QtEnumEditorFactory();
                browser->setFactoryForManager(enumManager, enumFactory);
            }
             else if (key == "ACT-FN-OUTPUT" || key == "ACT-FN-HIDDEN" || key == "IN-FN" || key == "TGT-FN")
            {
                QStringList actList;
                enumManager = new QtEnumPropertyManager(this);
                property = enumManager->addProperty(key);
                property->setValueToolTip(valueString);
                actList << "act-fn" << "act-linear" << "act-atan" << "act-tanh" << "act-gauss"<< "act-log" << "act-log-t";
                enumManager->setEnumNames(property, actList);
                int enumValue = actList.indexOf(valueString);
                if (bInsertValues)
                    enumManager->setValue(property, enumValue);
                QtEnumEditorFactory* enumFactory = new QtEnumEditorFactory();
                browser->setFactoryForManager(enumManager, enumFactory);
            }
            else if (key == "HISTORY" || key == "PLOT" || key == "COLLECT")
            {
                boolManager = new QtBoolPropertyManager(this);
                property = boolManager->addProperty(key);
                property->setValueToolTip(valueString);
                bool valueBool = false;
                if (valueString == "t")
                    valueBool = true;
               else if (valueString == "nil")
                    valueBool = false;
                if (bInsertValues)
                    boolManager->setValue(property, valueBool);
                QtCheckBoxFactory * checkBox = new QtCheckBoxFactory;
                browser->setFactoryForManager(boolManager, checkBox);
            }
            else if (key == "EPOCHS" || key == "DIM" || key == "N-INPUT-LAYER"
                 || key == "N-HIDDEN-LAYER"|| key == "HIDDEN-LAYERS" || key == "N-OUTPUT-LAYER")
            {
                intManager = new QtIntPropertyManager(this);
                property = intManager->addProperty(key);
                property->setValueToolTip(valueString);
                if (bInsertValues)
                    intManager->setValue(property, valueString.toInt());
                QtSpinBoxFactory * spinBox = new QtSpinBoxFactory();
                browser->setFactoryForManager(intManager, spinBox);
            }
            if(property)
                browser->addProperty(property);
        }
        browser->setMaximumHeight(ui.frameBrowser->height());
        browser->setMaximumWidth(ui.frameBrowser->width());
        browser->show();
    }
}

void ConfigurationWidget::slotPressClose()
{
    this->close();
}

void ConfigurationWidget::slotPressNew()
{
    browser->clear();
    if (settings)
        delete settings;

    settings = new LispSettings(this, configurationFile);

    this->populateTreeWidget(false);
}

void ConfigurationWidget::save()
{
    if (browser)
    {
        // open
        QString content = "(";
        for (QtProperty* property : browser->properties())
        {
            QString text = property->propertyName();
            QVariant value = property->valueText();
            QString valueString;
            QString line;

            if (content.length() > 1)
                content += "\n";

            // correct values true = t and false = nil
            valueString = value.toString();
            if (valueString == "True")
                valueString = "t";
            else if (valueString == "False")
                valueString = "nil";

            // correct double values
            if (text == "N-OUTPUT-TH"
            || text == "NN-LEARNING-RATE"
            || text == "NN-MOMENTUM-COEFF"
            || text == "NN-FINAL-TARGET-ERROR"
            || text == "NN-ABORT-ERROR")
            {
                // append lisp double
                if (valueString.indexOf("d0") < 0)
                {
                    // comma separated?
                    if (valueString.indexOf(".") >= 0)
                        valueString += "d0";
                    else
                        valueString += ".0d0";
                }
            }
            // correct values for following keys
            if (text == "IMAGEFILES-DIR" || text == "OUTFILES-DIR" || text == "INFILES-DIR"
                     || text == "DAT-EXT" || text == "IMG-EXT"
                     || text == "GNUPLOT-PATH" || text == "MARSHAL-DATAFILE" || text == "PRG-NAME"
                     || text == "TRAINING-INFILE" || text == "TRAINING-OUTFILE" || text == "TRAINING-ERRFILE"
                     || text == "VALIDATION-INFILE" || text == "VALIDATION-OUTFILE"
                     || text == "TRAINING-PLOT" || text == "VALIDATION-PLOT" || text == "ERROR-PLOT")
                valueString = "\"" + value.toString() + "\"";

            // create entry-line
            line = " :" + text + " " + valueString;
            content += line;
        }
        // close f
        content += ")\n";

        QFile file(this->configurationFile.toStdString().c_str());
        file.open(QIODevice::Truncate | QIODevice::WriteOnly);
        file.write(content.toStdString().c_str(), content.length());
        file.close();
    }
}

void ConfigurationWidget::slotPressSave()
{
    save();
}

void ConfigurationWidget::slotPressSaveAs()
{
    QFileDialog fileDialog;
    fileDialog.setDirectory(this->configurationFile);
    QString fileName = fileDialog.getSaveFileName();
    if (!fileName.isEmpty())
    {
        this->configurationFile = fileName;
        save();
    }
}

void ConfigurationWidget::slotPressLoad()
{
    if (settings)
        delete settings;

    settings = new LispSettings(this, configurationFile);

    this->populateTreeWidget(true);
}

QString ConfigurationWidget::filePath(void)
{
    return configurationFile;
}
