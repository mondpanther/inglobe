#!/usr/bin/env python3
"""
Simple script to create examples.xlsx using built-in libraries
"""
import zipfile
import os
from xml.etree.ElementTree import Element, SubElement, tostring
from xml.dom import minidom

def create_xlsx():
    # Create the necessary XML files for a basic XLSX

    # Create workbook.xml
    workbook_xml = '''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <sheets>
    <sheet name="Sheet1" sheetId="1" r:id="rId1"/>
  </sheets>
</workbook>'''

    # Create worksheet with data
    worksheet_xml = '''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
  <sheetData>
    <row r="1">
      <c r="A1" t="inlineStr"><is><t>id</t></is></c>
      <c r="B1" t="inlineStr"><is><t>name</t></is></c>
      <c r="C1" t="inlineStr"><is><t>origin_country</t></is></c>
      <c r="D1" t="inlineStr"><is><t>destination_country</t></is></c>
      <c r="E1" t="inlineStr"><is><t>amount</t></is></c>
      <c r="F1" t="inlineStr"><is><t>partner_country</t></is></c>
    </row>
    <row r="2">
      <c r="A2"><v>1</v></c>
      <c r="B2" t="inlineStr"><is><t>Project A</t></is></c>
      <c r="C2" t="inlineStr"><is><t>US</t></is></c>
      <c r="D2" t="inlineStr"><is><t>CN</t></is></c>
      <c r="E2"><v>1000</v></c>
      <c r="F2" t="inlineStr"><is><t>AR</t></is></c>
    </row>
    <row r="3">
      <c r="A3"><v>2</v></c>
      <c r="B3" t="inlineStr"><is><t>Project B</t></is></c>
      <c r="C3" t="inlineStr"><is><t>GB</t></is></c>
      <c r="D3" t="inlineStr"><is><t>IN</t></is></c>
      <c r="E3"><v>1500</v></c>
      <c r="F3" t="inlineStr"><is><t>EG</t></is></c>
    </row>
    <row r="4">
      <c r="A4"><v>3</v></c>
      <c r="B4" t="inlineStr"><is><t>Project C</t></is></c>
      <c r="C4" t="inlineStr"><is><t>FR</t></is></c>
      <c r="D4" t="inlineStr"><is><t>MX</t></is></c>
      <c r="E4"><v>2000</v></c>
      <c r="F4" t="inlineStr"><is><t>TH</t></is></c>
    </row>
    <row r="5">
      <c r="A5"><v>4</v></c>
      <c r="B5" t="inlineStr"><is><t>Project D</t></is></c>
      <c r="C5" t="inlineStr"><is><t>DE</t></is></c>
      <c r="D5" t="inlineStr"><is><t>ZA</t></is></c>
      <c r="E5"><v>2500</v></c>
      <c r="F5" t="inlineStr"><is><t>PL</t></is></c>
    </row>
    <row r="6">
      <c r="A6"><v>5</v></c>
      <c r="B6" t="inlineStr"><is><t>Project E</t></is></c>
      <c r="C6" t="inlineStr"><is><t>JP</t></is></c>
      <c r="D6" t="inlineStr"><is><t>KR</t></is></c>
      <c r="E6"><v>3000</v></c>
      <c r="F6" t="inlineStr"><is><t>CL</t></is></c>
    </row>
  </sheetData>
</worksheet>'''

    # Create content types
    content_types = '''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
  <Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>
</Types>'''

    # Create relationships
    rels_main = '''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>
</Relationships>'''

    rels_workbook = '''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>
</Relationships>'''

    # Create the XLSX file
    with zipfile.ZipFile('examples.xlsx', 'w', zipfile.ZIP_DEFLATED) as xlsx:
        xlsx.writestr('[Content_Types].xml', content_types)
        xlsx.writestr('_rels/.rels', rels_main)
        xlsx.writestr('xl/workbook.xml', workbook_xml)
        xlsx.writestr('xl/_rels/workbook.xml.rels', rels_workbook)
        xlsx.writestr('xl/worksheets/sheet1.xml', worksheet_xml)

    print("examples.xlsx created successfully!")

if __name__ == '__main__':
    create_xlsx()
