#include "mainwindow.h"
#include "./ui_mainwindow.h"
#include <QFile>
#include <QFileSystemModel>
#include <QFileDialog>
#include <QKeyEvent>
#include "tools.h"
#include <QMessageBox>
MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    //this->setCentralWidget(ui->centralwidget);
    QFileSystemModel *model=new QFileSystemModel(this);
    model->setRootPath(QDir::rootPath());
    ui->treeView->setModel(model);
    //ui->treeView->setRootIndex(model->setRootPath(currentFile));
}
void MainWindow::on_treeView_doubleClicked(const QModelIndex &index)
{
    QFile file(model->filePath(index));
    currentFile=model->filePath(index);
    if(file.open(QFile::ReadOnly | QFile::Text))
    {
        QTextStream in(&file);
        QString text = in.readAll();
        content=text;
        ui->plainTextEdit->setPlainText(text);
        file.close();
    }
}

MainWindow::~MainWindow(){
    delete ui;
}
void MainWindow::keyPressEvent(QKeyEvent *event){
    auto k=event->key();

    switch (k) {
        case (Qt::Key_F1):
            {
            auto command=ui->plainTextEdit_2->toPlainText();
            ui->label->setText("comand "+command);
            system(command.toStdString().c_str());
            break;}
        case (Qt::Key_F4):{//new file
            currentFile.clear();
            ui->plainTextEdit->setPlainText(QString());
            break;
        }
        case (Qt::Key_F5):{
            QString fname=QFileDialog::getOpenFileName(this,"Open the file");
            QFile file(fname);
            currentFile=fname;
            if(!file.open(QIODevice::ReadOnly | QFile::Text)){
                QMessageBox::warning(this,"Warning","Cannot open file:"+file.errorString());
                return;
            }
            setWindowTitle(fname);
            QTextStream in(&file);
            QString text=in.readAll();
            ui->plainTextEdit->setPlainText(text);
            file.close();
            break;
        }
        case(Qt::Key_F6):{
            QFile file(currentFile);
            if(!file.open(QFile::WriteOnly | QFile::Text)){
                QMessageBox::warning(this,"Warning","Cannot save file:"+file.errorString());
                return;
            }
            QTextStream out(&file);
            QString txt=ui->plainTextEdit->toPlainText();
            out<<txt;
            file.close();
            break;
        }
        case(Qt::Key_F7):{
            QString fname=QFileDialog::getSaveFileName(this,"Save file as");
            QFile file(fname);
            if(!file.open(QFile::WriteOnly | QFile::Text)){
                QMessageBox::warning(this,"Warning","Cannot save file:"+file.errorString());
                return;
            }
            currentFile=fname;
            setWindowTitle(fname);
            QTextStream out(&file);
            QString txt=ui->plainTextEdit->toPlainText();
            out<<txt;
            file.close();
            break;
        }

    }

}

void MainWindow::on_actionNew_triggered(){
    currentFile.clear();
    ui->plainTextEdit->setPlainText(QString());
}


void MainWindow::on_actionOpen_triggered(){
    QString fname=QFileDialog::getOpenFileName(this,"Open the file");
    QFile file(fname);
    currentFile=fname;
    if(!file.open(QIODevice::ReadOnly | QFile::Text)){
        QMessageBox::warning(this,"Warning","Cannot open file:"+file.errorString());
        return;
    }
    setWindowTitle(fname);
    QTextStream in(&file);
    QString text=in.readAll();
    ui->plainTextEdit->setPlainText(text);
    //ui->treeView->setRootIndex(model->setRootPath(fname));
    file.close();
}



void MainWindow::on_actionSave_triggered(){
    QFile file(currentFile);
    if(!file.open(QFile::WriteOnly | QFile::Text)){
        QMessageBox::warning(this,"Warning","Cannot save file:"+file.errorString());
        return;
    }
    QTextStream out(&file);
    QString txt=ui->plainTextEdit->toPlainText();
    out<<txt;
    file.close();
}


void MainWindow::on_actionSave_as_triggered(){
    QString fname=QFileDialog::getSaveFileName(this,"Save file as");
    QFile file(fname);
    if(!file.open(QFile::WriteOnly | QFile::Text)){
        QMessageBox::warning(this,"Warning","Cannot save file:"+file.errorString());
        return;
    }
    currentFile=fname;
    setWindowTitle(fname);
    QTextStream out(&file);
    QString txt=ui->plainTextEdit->toPlainText();
    out<<txt;
    file.close();
}


void MainWindow::on_treeView_clicked(const QModelIndex &index)
{

}

