package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;

import org.cyk.utility.__kernel__.persistence.query.CountQueryIdentifierGetter;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ClusterPrivilegesQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ClusterQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.MenuQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestDispatchSlipQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ServiceQuerier;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class CountQueryIdentifierGetterImpl extends CountQueryIdentifierGetter.AbstractImpl implements Serializable {

	@Override
	protected String __get__(String readQueryIdentifier) {
		if(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT.equals(readQueryIdentifier))
			return ExecutionImputationQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		if(ServiceQuerier.QUERY_IDENTIFIER_READ_WITH_ALL.equals(readQueryIdentifier))
			return ServiceQuerier.QUERY_IDENTIFIER_COUNT;
		if(MenuQuerier.QUERY_IDENTIFIER_READ_WITH_ALL.equals(readQueryIdentifier))
			return MenuQuerier.QUERY_IDENTIFIER_COUNT;
		if(MenuQuerier.QUERY_IDENTIFIER_READ_WITH_ALL_BY_SERVICE_IDENTIFIER.equals(readQueryIdentifier))
			return MenuQuerier.QUERY_IDENTIFIER_COUNT_BY_SERVICE_IDENTIFIER;
		if(MenuQuerier.QUERY_IDENTIFIER_READ_WITH_ALL_BY_SERVICE_CODE.equals(readQueryIdentifier))
			return MenuQuerier.QUERY_IDENTIFIER_COUNT_BY_SERVICE_CODE;
		if(FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_PROFILES.equals(readQueryIdentifier))
			return FunctionQuerier.QUERY_IDENTIFIER_COUNT;
		if(FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_PROFILES_BY_TYPES_CODES.equals(readQueryIdentifier))
			return FunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES;
		if(FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_ALL_BY_TYPE_IDENTIFIER.equals(readQueryIdentifier))
			return FunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_TYPE_IDENTIFIER;
		if(ActorQuerier.QUERY_IDENTIFIER_READ_WITH_FUNCTIONS_WHERE_FILTER.equals(readQueryIdentifier))
			return ActorQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		if(ActorQuerier.QUERY_IDENTIFIER_READ_WITH_ALL_WHERE_FILTER.equals(readQueryIdentifier))
			return ActorQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		if(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_WITH_ALL.equals(readQueryIdentifier))
			return ExecutionImputationQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		if(ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ_FOR_UI_BY_FUNCTIONS_IDENTIFIERS.equals(readQueryIdentifier))
			return ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS;
		if(ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ_FOR_UI.equals(readQueryIdentifier))
			return ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_COUNT;
		
		if(AdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI.equals(readQueryIdentifier))
			return AdministrativeUnitQuerier.QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER;
		if(AdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS_FOR_UI.equals(readQueryIdentifier))
			return AdministrativeUnitQuerier.QUERY_IDENTIFIER_COUNT_BY_SECTIONS_IDENTIFIERS;
		
		if(BudgetSpecializationUnitQuerier.QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI.equals(readQueryIdentifier))
			return BudgetSpecializationUnitQuerier.QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER;
		
		if(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_APPLY_MODEL.equals(readQueryIdentifier))
			return AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		if(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(readQueryIdentifier))
			return AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		if(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT.equals(readQueryIdentifier))
			return AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		
		if(AssignmentsQuerier.QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI.equals(readQueryIdentifier))
			return AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_FULLY_ASSIGNED_WHERE_FILTER;
		if(AssignmentsQuerier.QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI.equals(readQueryIdentifier))
			return AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_NOT_FULLY_ASSIGNED_WHERE_FILTER;
		
		if(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(readQueryIdentifier))
			return ScopeFunctionQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		
		if(ClusterQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(readQueryIdentifier))
			return ClusterQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		
		if(ClusterPrivilegesQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(readQueryIdentifier))
			return ClusterPrivilegesQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		
		if(RequestQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(readQueryIdentifier))
			return RequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		
		if(RequestDispatchSlipQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(readQueryIdentifier))
			return RequestDispatchSlipQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
		
		return super.__get__(readQueryIdentifier);
	}
}