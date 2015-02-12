module MM_Node where

import List (..)
import Array (..)

-- Schema for node from freeplane
-- <xs:element name='node'>
--  <xs:complexType>
--   <xs:choice minOccurs='0' maxOccurs='unbounded'>
--    <xs:element ref='arrowlink'/>
--    <xs:element ref='cloud'/>
--    <xs:element ref='edge'/>
--    <xs:element ref='font'/>
--    <xs:element ref='hook'/>
--    <xs:element ref='icon'/>
--    <xs:element ref='node'/>
--    <!-- For nodes with extended formatting content or for notes to nodes. -->
--    <xs:element ref='richcontent'/>
--    <xs:element ref='attribute_layout'/>
--    <xs:element ref='attribute'/>
--   </xs:choice>
--   <xs:attribute name='BACKGROUND_COLOR' type='xs:string' use='optional'/>
--   <xs:attribute name='COLOR' type='xs:string' use='optional'/>
--   <xs:attribute name='FOLDED' use='optional'>
--    <xs:simpleType>
--     <xs:restriction base='xs:string'>
--      <xs:enumeration value='true'/>
--      <xs:enumeration value='false'/>
--     </xs:restriction>
--    </xs:simpleType>
--   </xs:attribute>
--   <xs:attribute name='ID' type='xs:ID' use='optional'/>
--   <xs:attribute name='LINK' type='xs:string' use='optional'/>
--   <xs:attribute name='POSITION' use='optional'>
--    <xs:simpleType>
--     <xs:restriction base='xs:string'>
--      <xs:enumeration value='left'/>
--      <xs:enumeration value='right'/>
--     </xs:restriction>
--    </xs:simpleType>
--   </xs:attribute>
--   <xs:attribute name='STYLE' type='xs:string' use='optional'/>
--   <xs:attribute name='TEXT' type='xs:string' use='optional'/>
--   <xs:attribute name='LOCALIZED_TEXT' type='xs:string' use='optional'/>
--   <xs:attribute name='TYPE' type='xs:string' use='optional'/>
--   <xs:attribute name='CREATED' type='xs:integer' use='optional'/>
--   <xs:attribute name='MODIFIED' type='xs:integer' use='optional'/>
--   <xs:attribute name='HGAP' type='xs:integer' use='optional'/>
--   <xs:attribute name='VGAP' type='xs:integer' use='optional'/>
--   <xs:attribute name='VSHIFT' type='xs:integer' use='optional'/>
--   <xs:attribute name='ENCRYPTED_CONTENT' type='xs:string' use='optional'/>
--  </xs:complexType>
-- </xs:element>


type alias MM_Node = 
    {  nodeName   : String
    ,  text       : String
    ,  folded     : Bool
    ,  id         : Int
--    ,  style      : MM_NodeStyle
    } 

-- Some APIs to work with Model
-- Create a new node with given id
newMM_Node : String -> Int-> MM_Node
newMM_Node name i = 
    {  nodeName   = name
    ,  text       = ""
    ,  folded     = False
    ,  id         = i
    }

getNodeID : MM_Node -> Int
getNodeID n = n.id

-- type MM_NodeStyle = NoStyle
