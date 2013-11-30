package net.binarythink.scalex

import scala.swing._
import scala.swing.event._

object ScalexAPP extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Scalex"
      val btnDefinition = new Button {
        text = "..."
      }
      val btnSource = new Button {
        text = "..."
      }
      val btnDestination = new Button {
        text = "..."
      }
      val btnGenerate = new Button {
        text = "Gen"
      }
      val btnAbout = new Button {
        text = "About"
      }
      
      val txtDefinition = new TextField {
        columns = 20
      }
      val txtSource = new TextField {
        columns = 20
      }
      val txtDestination = new TextField {
        columns = 20
      }
      val lblStatus = new Label{
        text = "Made by lichenbo(111250075)"
      }
      
      val definitionFileChooser = new FileChooser()
      definitionFileChooser.fileFilter = new javax.swing.filechooser.FileNameExtensionFilter("Definition File(.xml)","xml")
      val sourceFileChooser = new FileChooser()
      val destinationFileChooser = new FileChooser
      destinationFileChooser.fileFilter = new javax.swing.filechooser.FileNameExtensionFilter("Token Sequence(.token)","token")

    contents = new GridBagPanel {
      import swing.GridBagPanel._
      val lblTitle = new Label {
        text = "<html><center><h1>Scalex</h1>a simple lexer made with<h3>Scala programming language</h3></center>"
        horizontalTextPosition = Alignment.Center
      }
      
      val lblDefinition = new Label {
        text = "<html>Syntax File:"
      }
      
      val lblSource = new Label {
        text = "<html>Source File:"
      }
      
      val lblDestination = new Label {
        text = "<html>Destination Path:"
      }

      val c = new Constraints
      c.insets = new Insets(5,5,5,5)
      border = Swing.EmptyBorder(30, 30, 10, 30)
      
      c.gridx = 0
      c.gridy = 0
      c.gridwidth = 5
      layout(lblTitle) = c
      
      c.gridx = 0
      c.gridy = 1
      c.gridwidth = 1
      layout(lblDefinition) = c
      c.gridx = 1
      layout(txtDefinition) = c
      c.gridx = 2
      layout(btnDefinition) = c
      
      c.gridy = 2
      c.gridx = 0
      layout(lblSource) = c
      c.gridx = 1
      layout(txtSource) = c
      c.gridx = 2
      layout(btnSource) = c
      
      c.gridy = 3
      c.gridx = 0
      layout(lblDestination) = c
      c.gridx = 1
      layout(txtDestination) = c
      c.gridx = 2
      layout(btnDestination) = c
      
      c.gridy = 4
      c.gridx = 2
      layout(btnGenerate) = c
      
      c.gridx = 1
      layout(lblStatus) = c
    }
    listenTo(btnDefinition,btnSource,btnDestination,btnGenerate)
    reactions += {
      case ButtonClicked(`btnDefinition`) =>
        if(definitionFileChooser.showOpenDialog(contents(0)) == FileChooser.Result.Approve)
          txtDefinition.text = definitionFileChooser.selectedFile.getAbsolutePath()
      case ButtonClicked(`btnSource`) =>
        if(sourceFileChooser.showOpenDialog(contents(0)) == FileChooser.Result.Approve)
          txtSource.text = sourceFileChooser.selectedFile.getAbsolutePath()
      case ButtonClicked(`btnDestination`) =>
        if(destinationFileChooser.showSaveDialog(contents(0)) == FileChooser.Result.Approve)
          txtDestination.text = destinationFileChooser.selectedFile.getAbsolutePath()
      case ButtonClicked(`btnGenerate`) =>
        if (txtDefinition.text == "" || txtSource.text == "" || txtDestination.text == "")
          Dialog.showMessage(contents(0), "Cannot be empty", "Empty box", Dialog.Message.Error)
        else {
          lblStatus.text = "Generaing %s, please wait... ".format(txtDestination.text)
          Scalex.main(Array(txtDefinition.text,txtSource.text,txtDestination.text))
          lblStatus.text = "Generation Complete!"
        }
    }
    
  }

}