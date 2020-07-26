package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetaryFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RejectedAccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntityReaderImpl extends EntityReader.AbstractImpl implements Serializable {

	@SuppressWarnings("unchecked")
	@Override
	public <T> T readOne(Class<T> tupleClass, QueryExecutorArguments arguments) {
		if(arguments != null && arguments.getQuery() != null) {
			if(ActorQuerier.QUERY_IDENTIFIER_READ_ONE_WITH_ALL_PRIVILEGES_BY_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return (T) ActorQuerier.getInstance().readOneWithAllPrivilegesByIdentifier((String)arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_IDENTIFIER));
		}
		return super.readOne(tupleClass, arguments);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public <T> Collection<T> readMany(Class<T> tupleClass, QueryExecutorArguments arguments) {
		if(arguments != null) {
			if(PrivilegeQuerier.QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) PrivilegeQuerier.getInstance().readVisibleByActorCode((String)arguments.getFilterFieldValue(PrivilegeQuerier.PARAMETER_NAME_ACTOR_CODE));
			if(FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_PROFILES.equals(arguments.getQuery().getIdentifier())
					|| FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_PROFILES_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier())) {	
				if(FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_PROFILES.equals(arguments.getQuery().getIdentifier()))
					arguments.getQuery().setIdentifier(FunctionQuerier.QUERY_IDENTIFIER_READ);
				else if(FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_PROFILES_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier()))
					arguments.getQuery().setIdentifier(FunctionQuerier.QUERY_IDENTIFIER_READ_BY_TYPES_CODES);
				Collection<Function> functions = (Collection<Function>) super.readMany(tupleClass, arguments);
				if(CollectionHelper.isEmpty(functions))
					return null;
				Collection<ProfileFunction> profileFunctions = ProfileFunctionQuerier.getInstance().readByFunctionsCodes(functions.stream().map(x -> x.getCode())
						.collect(Collectors.toList()));
				if(CollectionHelper.isNotEmpty(profileFunctions))
					functions.forEach(function -> {
						Collection<ProfileFunction> __profileFunctions__ = profileFunctions.stream().filter(profileFunction -> profileFunction.getFunction().equals(function)).collect(Collectors.toList());
						if(CollectionHelper.isNotEmpty(__profileFunctions__))
							function.setProfilesAsStrings(__profileFunctions__.stream().map(profileFunction -> profileFunction.getProfile().getName()).collect(Collectors.toList()));
					});
				return (Collection<T>) functions;
			}
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readWhereTypeIsUAAndFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readVisibleSectionsWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_INVISIBLE_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readInvisibleSectionsWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readVisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readInvisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_NOT_ASSOCIATED.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ScopeQuerier.getInstance().readWhereFilterNotAssociated(arguments);
			
			if(AccountRequestQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) AccountRequestQuerier.getInstance().readWhereFilter(arguments);
			if(ActorQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) ActorQuerier.getInstance().readWhereFilter(arguments);
			
			if(RejectedAccountRequestQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) RejectedAccountRequestQuerier.getInstance().readWhereFilter(arguments);
			
			if(AdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) AdministrativeUnitQuerier.getInstance().readWhereCodeOrNameLike(arguments);
			
			if(Boolean.TRUE.equals(FunctionQuerier.getInstance().isOwner(arguments)))
				return (Collection<T>) FunctionQuerier.getInstance().readMany(arguments);
			
			if(Boolean.TRUE.equals(BudgetaryFunctionQuerier.getInstance().isOwner(arguments)))
				return (Collection<T>) BudgetaryFunctionQuerier.getInstance().readMany(arguments);
		}
		return super.readMany(tupleClass, arguments);
	}
}