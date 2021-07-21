package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;

import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ClusterQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RejectedAccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestDispatchSlipQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntityCounterImpl extends org.cyk.utility.persistence.server.query.EntityCounterImpl implements Serializable {

	@Override
	public Long count(Class<?> tupleClass, QueryExecutorArguments arguments) {
		if(ScopeOfTypeSectionQuerier.isProcessable(arguments))
			return ScopeOfTypeSectionQuerier.getInstance().count(arguments);
		if(ScopeOfTypeAdministrativeUnitQuerier.isProcessable(arguments))
			return ScopeOfTypeAdministrativeUnitQuerier.getInstance().count(arguments);
		if(ScopeOfTypeBudgetSpecializationUnitQuerier.isProcessable(arguments))
			return ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().count(arguments);
		if(ScopeOfTypeActionQuerier.isProcessable(arguments))
			return ScopeOfTypeActionQuerier.getInstance().count(arguments);
		if(ScopeOfTypeActivityQuerier.isProcessable(arguments))
			return ScopeOfTypeActivityQuerier.getInstance().count(arguments);
		if(ScopeOfTypeImputationQuerier.isProcessable(arguments))
			return ScopeOfTypeImputationQuerier.getInstance().count(arguments);
		
		if(Boolean.TRUE.equals(QueryIdentifierBuilder.builtFrom(arguments, ScopeType.class)))
			return ScopeTypeQuerier.getInstance().count(arguments);
		
		if(Boolean.TRUE.equals(QueryIdentifierBuilder.builtFrom(arguments, Scope.class)))
			return ScopeQuerier.getInstance().count(arguments);
		
		if(Boolean.TRUE.equals(ClusterQuerier.getInstance().isOwner(arguments)))
			return ClusterQuerier.getInstance().count(arguments);
		
		if(Boolean.TRUE.equals(QueryIdentifierBuilder.builtFrom(arguments, RequestType.class)))
			return RequestTypeQuerier.getInstance().count(arguments);
		
		if(Boolean.TRUE.equals(QueryIdentifierBuilder.builtFrom(arguments, Request.class)))
			return RequestQuerier.getInstance().count(arguments);
		
		if(Boolean.TRUE.equals(QueryIdentifierBuilder.builtFrom(arguments, RequestDispatchSlip.class)))
			return RequestDispatchSlipQuerier.getInstance().count(arguments);
		
		if(Boolean.TRUE.equals(QueryIdentifierBuilder.builtFrom(arguments, Assignments.class)))
			return AssignmentsQuerier.getInstance().count(arguments);
		
		if(arguments != null && arguments.getQuery() != null) {
			if(ActorQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return ActorQuerier.getInstance().countWhereFilter(arguments);
			
			if(AccountRequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return AccountRequestQuerier.getInstance().countWhereFilter(arguments);
			if(RejectedAccountRequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return RejectedAccountRequestQuerier.getInstance().countWhereFilter(arguments);
			
			if(Boolean.TRUE.equals(ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier())))
				return ScopeTypeFunctionQuerier.getInstance().count();
			if(Boolean.TRUE.equals(ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS.equals(arguments.getQuery().getIdentifier())))
				return ScopeTypeFunctionQuerier.getInstance().countByFunctionsIdentifiers(arguments);
			
			if(Boolean.TRUE.equals(ScopeFunctionQuerier.getInstance().isOwner(arguments)))
				return ScopeFunctionQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(ServiceQuerier.getInstance().isOwner(arguments)))
				return ServiceQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(ProfileQuerier.getInstance().isOwner(arguments)))
				return ProfileQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(FunctionQuerier.getInstance().isOwner(arguments)))
				return FunctionQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(SectionQuerier.getInstance().isOwner(arguments)))
				return SectionQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(BudgetSpecializationUnitQuerier.getInstance().isOwner(arguments)))
				return BudgetSpecializationUnitQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(AdministrativeUnitQuerier.getInstance().isOwner(arguments)))
				return AdministrativeUnitQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(ExecutionImputationQuerier.getInstance().isOwner(arguments)))
				return ExecutionImputationQuerier.getInstance().count(arguments);
		}
		
		if(Boolean.TRUE.equals(QueryIdentifierBuilder.builtFrom(arguments, Actor.class)))
			return ActorQuerier.getInstance().count(arguments);
		
		return super.count(tupleClass, arguments);
	}
}