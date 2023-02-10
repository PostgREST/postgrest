{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PostgREST.TypeHelper where

import PostgREST.ApiRequest             (Action (..), ApiRequest (..),
                                         InvokeMethod (..),
                                         Mutation (..), PathInfo (..),
                                         Payload (..),
                                         RpcParamValue (..),
                                         Target (..))
import PostgREST.ApiRequest.Preferences (PreferCount (..),
                                         PreferParameters (..),
                                         PreferRepresentation (..),
                                         PreferResolution (..),
                                         PreferTransaction (..),
                                         Preferences (..))
import PostgREST.ApiRequest.QueryParams (QueryParams (..))
import PostgREST.ApiRequest.Types       (ApiRequestError (..),
                                         EmbedParam (..), Filter (..),
                                         FtsOperator (..),
                                         JoinType (..),
                                         JsonOperand (..),
                                         JsonOperation (..),
                                         LogicOperator (..),
                                         LogicTree (..), OpExpr (..),
                                         Operation (..),
                                         OrderDirection (..),
                                         OrderNulls (..),
                                         OrderTerm (..), QPError (..),
                                         RangeError (..),
                                         SelectItem (..),
                                         SimpleOperator (..),
                                         TrileanVal (..))

--import PostgREST.AppState (AppState (..))

import PostgREST.Auth (AuthResult (..))

import PostgREST.CLI (CLI (..), Command (..))

import PostgREST.Config           (AppConfig (..), LogLevel (..),
                                   OpenAPIMode (..))
import PostgREST.Config.JSPath    (JSPathExp (..))
import PostgREST.Config.PgVersion (PgVersion (..))
import PostgREST.Config.Proxy     (Proxy (..))

import PostgREST.Error (Error (..), ErrorCode (..), PgError (..))

import PostgREST.MediaType (MTPlanAttrs (..), MTPlanFormat (..),
                            MTPlanOption (..), MediaType (..))

import PostgREST.Plan            (CallReadPlan (..),
                                  MutateReadPlan (..))
import PostgREST.Plan.CallPlan   (CallParams (..), CallPlan (..))
import PostgREST.Plan.MutatePlan (MutatePlan (..))
import PostgREST.Plan.ReadPlan   (JoinCondition (..), ReadPlan (..))
import PostgREST.Plan.Types      (TypedField (..))

import PostgREST.Query.Statements (ResultSet (..))

import PostgREST.Response.GucHeader (GucHeader (..))

import PostgREST.SchemaCache              (KeyDep (..),
                                           SchemaCache (..),
                                           ViewKeyDependency (..))
import PostgREST.SchemaCache.Identifiers  (QualifiedIdentifier (..))
import PostgREST.SchemaCache.Proc         (PgType (..),
                                           ProcDescription (..),
                                           ProcParam (..),
                                           ProcVolatility (..),
                                           RetType (..))
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..))
import PostgREST.SchemaCache.Table        (Column (..), Table (..))

import PostgREST.Workers (ConnectionStatus (..), SCacheStatus (..))

import Protolude (Show)

-- ApiRequest
deriving instance Show Payload
deriving instance Show InvokeMethod
deriving instance Show Mutation
deriving instance Show Action
deriving instance Show PathInfo
deriving instance Show Target
deriving instance Show RpcParamValue
deriving instance Show ApiRequest

-- ApiRequest.Preferences
deriving instance Show PreferResolution
deriving instance Show PreferRepresentation
deriving instance Show PreferParameters
deriving instance Show PreferCount
deriving instance Show PreferTransaction
deriving instance Show Preferences

-- ApiRequest.QueryParams
deriving instance Show QueryParams

-- ApiRequest.Types
deriving instance Show SelectItem
deriving instance Show ApiRequestError
deriving instance Show QPError
deriving instance Show RangeError
deriving instance Show OrderTerm
deriving instance Show OrderDirection
deriving instance Show OrderNulls
deriving instance Show EmbedParam
deriving instance Show JoinType
deriving instance Show JsonOperation
deriving instance Show JsonOperand
deriving instance Show LogicTree
deriving instance Show LogicOperator
deriving instance Show Filter
deriving instance Show OpExpr
deriving instance Show Operation
deriving instance Show TrileanVal
deriving instance Show SimpleOperator
deriving instance Show FtsOperator

-- AppState
--deriving instance Show AppState

-- Auth
deriving instance Show AuthResult

-- CLI
deriving instance Show CLI
deriving instance Show Command

-- Config
deriving instance Show AppConfig
deriving instance Show LogLevel
deriving instance Show OpenAPIMode

-- Config.JSPath
deriving instance Show JSPathExp

-- Config.PgVersion
deriving instance Show PgVersion

-- Config.Proxy
deriving instance Show Proxy

-- Error
deriving instance Show PgError
deriving instance Show Error
deriving instance Show ErrorCode

-- MediaType
deriving instance Show MediaType
deriving instance Show MTPlanAttrs
deriving instance Show MTPlanOption
deriving instance Show MTPlanFormat

-- Plan
deriving instance Show MutateReadPlan
deriving instance Show CallReadPlan

-- Plan.CallPlan
deriving instance Show CallPlan
deriving instance Show CallParams

-- Plan.MutatePlan
deriving instance Show MutatePlan

-- Plan.ReadPlan
deriving instance Show JoinCondition
deriving instance Show ReadPlan

-- Plan.Types
deriving instance Show TypedField

-- Query.Statements
deriving instance Show ResultSet

-- Response.GucHeaders
deriving instance Show GucHeader

-- SchemaCache
deriving instance Show SchemaCache
deriving instance Show ViewKeyDependency
deriving instance Show KeyDep

-- SchemaCache.Identifiers
deriving instance Show QualifiedIdentifier

-- SchemaCache.Procs
deriving instance Show PgType
deriving instance Show RetType
deriving instance Show ProcVolatility
deriving instance Show ProcDescription
deriving instance Show ProcParam

-- SchemaCache.Relationships
deriving instance Show Relationship
deriving instance Show Cardinality
deriving instance Show Junction

-- SchemaCache.Table
deriving instance Show Table
deriving instance Show Column

-- Workers
deriving instance Show ConnectionStatus
deriving instance Show SCacheStatus
