package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActivityCategoryQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetaryFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.MenuQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RejectedAccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityCategoryQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ServiceQuerier;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntityReaderImpl extends EntityReader.AbstractImpl implements Serializable {

	@SuppressWarnings("unchecked")
	@Override
	public <T> T readOne(Class<T> tupleClass, QueryExecutorArguments arguments) {
		if(arguments != null && arguments.getQuery() != null) {
			if(ActorQuerier.QUERY_IDENTIFIER_READ_ONE_WITH_ALL_PRIVILEGES_BY_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return (T) ActorQuerier.getInstance().readOneWithAllPrivilegesByIdentifier((String)arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_IDENTIFIER));
			if(AccountRequestQuerier.QUERY_IDENTIFIER_READ_PROJECTION_01_WITH_BUDGETARIES_FUNCTIONS_AND_FUNCTIONS_BY_ACCESS_TOKEN.equals(arguments.getQuery().getIdentifier()))
				return (T) AccountRequestQuerier.getInstance().readProjection01WithBudgetaryFunctionsAndFunctionsByAccessToken((String)arguments
						.getFilterFieldValue(AccountRequestQuerier.PARAMETER_NAME_ACCESS_TOKEN));
			if(AccountRequestQuerier.QUERY_IDENTIFIER_READ_PROJECTION_01_WITH_BUDGETARIES_FUNCTIONS_AND_FUNCTIONS_BY_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return (T) AccountRequestQuerier.getInstance().readProjection01WithBudgetaryFunctionsAndFunctionsByIdentifier((String)arguments
						.getFilterFieldValue(AccountRequestQuerier.PARAMETER_NAME_IDENTIFIER));
			if(AccountRequestQuerier.QUERY_IDENTIFIER_READ_PROJECTION_02_WITH_BUDGETARIES_FUNCTIONS_AND_FUNCTIONS_BY_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return (T) AccountRequestQuerier.getInstance().readProjection02WithBudgetaryFunctionsAndFunctionsByIdentifier((String)arguments
						.getFilterFieldValue(AccountRequestQuerier.PARAMETER_NAME_IDENTIFIER));
			if(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT.equals(arguments.getQuery().getIdentifier()))
				return (T) ExecutionImputationQuerier.getInstance().readByIdentifierForEdit((String)arguments
						.getFilterFieldValue(ExecutionImputationQuerier.PARAMETER_NAME_IDENTIFIER));
		}
		return super.readOne(tupleClass, arguments);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public <T> Collection<T> readMany(Class<T> tupleClass, QueryExecutorArguments arguments) {
		if(ScopeOfTypeSectionQuerier.isProcessable(arguments))
			return (Collection<T>) ScopeOfTypeSectionQuerier.getInstance().readMany(arguments);
		if(ScopeOfTypeAdministrativeUnitQuerier.isProcessable(arguments))
			return (Collection<T>) ScopeOfTypeAdministrativeUnitQuerier.getInstance().readMany(arguments);
		if(ScopeOfTypeBudgetSpecializationUnitQuerier.isProcessable(arguments))
			return (Collection<T>) ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().readMany(arguments);
		if(ScopeOfTypeActionQuerier.isProcessable(arguments))
			return (Collection<T>) ScopeOfTypeActionQuerier.getInstance().readMany(arguments);
		if(ScopeOfTypeActivityQuerier.isProcessable(arguments))
			return (Collection<T>) ScopeOfTypeActivityQuerier.getInstance().readMany(arguments);
		
		if(ScopeOfTypeActivityCategoryQuerier.isProcessable(arguments))
			return (Collection<T>) ScopeOfTypeActivityCategoryQuerier.getInstance().readMany(arguments);	
		
		if(ScopeOfTypeImputationQuerier.isProcessable(arguments))
			return (Collection<T>) ScopeOfTypeImputationQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(ScopeFunctionQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) ScopeFunctionQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(ServiceQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) ServiceQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(MenuQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) MenuQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(FunctionQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) FunctionQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(ProfileQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) ProfileQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(BudgetaryFunctionQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) BudgetaryFunctionQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(SectionQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) SectionQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(ActivityCategoryQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) ActivityCategoryQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(BudgetSpecializationUnitQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) BudgetSpecializationUnitQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(AdministrativeUnitQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) AdministrativeUnitQuerier.getInstance().readMany(arguments);
		
		if(Boolean.TRUE.equals(ExecutionImputationQuerier.getInstance().isOwner(arguments)))
			return (Collection<T>) ExecutionImputationQuerier.getInstance().readMany(arguments);
		
		if(arguments != null && arguments.getQuery() != null) {
			if(Boolean.TRUE.equals(ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier())))
				return (Collection<T>) ScopeTypeFunctionQuerier.getInstance().read();
			if(Boolean.TRUE.equals(ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ_FOR_UI.equals(arguments.getQuery().getIdentifier())))
				return (Collection<T>) ScopeTypeFunctionQuerier.getInstance().readForUI();
			if(Boolean.TRUE.equals(ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS.equals(arguments.getQuery().getIdentifier())))
				return (Collection<T>) ScopeTypeFunctionQuerier.getInstance().readByFunctionsIdentifiers(arguments);
			if(Boolean.TRUE.equals(ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ_FOR_UI_BY_FUNCTIONS_IDENTIFIERS.equals(arguments.getQuery().getIdentifier())))
				return (Collection<T>) ScopeTypeFunctionQuerier.getInstance().readForUIByFunctionsIdentifiers(arguments);
			
			if(PrivilegeQuerier.QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) PrivilegeQuerier.getInstance().readVisibleByActorCode((String)arguments.getFilterFieldValue(PrivilegeQuerier.PARAMETER_NAME_ACTOR_CODE));
			
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readWhereTypeIsUAAndFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_USB_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readWhereTypeIsUSBAndFilter(arguments);
			
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_NOT_ASSOCIATED.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readWhereFilterNotAssociated(arguments);
			
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(arguments);
			
			if(AccountRequestQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) AccountRequestQuerier.getInstance().readWhereFilter(arguments);
			
			if(ActorQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ActorQuerier.getInstance().readWhereFilter(arguments);
			if(ActorQuerier.QUERY_IDENTIFIER_READ_WITH_FUNCTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ActorQuerier.getInstance().readWithFunctionsWhereFilter(arguments);
			if(ActorQuerier.QUERY_IDENTIFIER_READ_WITH_ALL_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ActorQuerier.getInstance().readWithAllWhereFilter(arguments);
			
			if(RejectedAccountRequestQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) RejectedAccountRequestQuerier.getInstance().readWhereFilter(arguments);
			
		}
		return super.readMany(tupleClass, arguments);
	}
}