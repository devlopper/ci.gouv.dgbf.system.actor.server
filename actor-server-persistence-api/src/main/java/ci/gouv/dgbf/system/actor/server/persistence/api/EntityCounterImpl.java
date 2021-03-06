package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;

import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetaryFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RejectedAccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntityCounterImpl extends EntityCounter.AbstractImpl implements Serializable {

	@Override
	public <T> Long count(Class<T> tupleClass, QueryExecutorArguments arguments) {
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
		
		if(arguments != null && arguments.getQuery() != null) {
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return ScopeQuerier.getInstance().countWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return ScopeQuerier.getInstance().countWhereTypeIsUAAndFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_USB_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return ScopeQuerier.getInstance().countWhereTypeIsUSBAndFilter(arguments);
			
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER_NOT_ASSOCIATED.equals(arguments.getQuery().getIdentifier()))
				return ScopeQuerier.getInstance().countWhereFilterNotAssociated(arguments);
			
			if(ActorQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return ActorQuerier.getInstance().countWhereFilter(arguments);
			
			if(AccountRequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return AccountRequestQuerier.getInstance().countWhereFilter(arguments);
			if(RejectedAccountRequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return RejectedAccountRequestQuerier.getInstance().countWhereFilter(arguments);
			
			if(Boolean.TRUE.equals(ProfileQuerier.getInstance().isOwner(arguments)))
				return ProfileQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(FunctionQuerier.getInstance().isOwner(arguments)))
				return FunctionQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(BudgetaryFunctionQuerier.getInstance().isOwner(arguments)))
				return BudgetaryFunctionQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(SectionQuerier.getInstance().isOwner(arguments)))
				return SectionQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(BudgetSpecializationUnitQuerier.getInstance().isOwner(arguments)))
				return BudgetSpecializationUnitQuerier.getInstance().count(arguments);
			
			if(Boolean.TRUE.equals(AdministrativeUnitQuerier.getInstance().isOwner(arguments)))
				return AdministrativeUnitQuerier.getInstance().count(arguments);
		}
		return super.count(tupleClass, arguments);
	}
}