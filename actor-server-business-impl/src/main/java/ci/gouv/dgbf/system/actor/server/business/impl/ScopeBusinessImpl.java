package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.business.server.AbstractSpecificBusinessImpl;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeAdministrativeUnitSectionsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeBudgetSpecializationUnitSectionsReader;

@ApplicationScoped
public class ScopeBusinessImpl extends AbstractSpecificBusinessImpl<Scope> implements ScopeBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected Class<Scope> getEntityClass() {
		return Scope.class;
	}
	
	@Override
	public Collection<Scope> get(String typeCode, String actorCode, Boolean visible, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples
			,Boolean removeTypeCodeFromIdentifier) {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(getEntityClass())
				.filterIfNotBlank(ScopeQuerier.PARAMETER_NAME_TYPE_CODE, typeCode)
				.filterIfNotBlank(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)
				.filterIfNotNull(ScopeQuerier.PARAMETER_NAME_VISIBLE,visible)
				.page(pageable, firstTupleIndex, numberOfTuples);
		Collection<Scope> scopes = EntityReader.getInstance().readMany(getEntityClass(), arguments);
		if(CollectionHelper.isEmpty(scopes))
			return null;
		if(Boolean.TRUE.equals(removeTypeCodeFromIdentifier))
			scopes.forEach(scope -> {
				scope.setIdentifier(StringUtils.substringAfter(scope.getIdentifier(), typeCode));
			});
		return scopes;
	}
	
	@Override
	public Collection<Scope> getByActorCode(String actorCode, String typeCode, Boolean visible, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples
			, Boolean removeTypeCodeFromIdentifier) {
		CodeExecutor.getInstance().throwExceptionIfNotExist(Actor.class, actorCode);
		CodeExecutor.getInstance().throwExceptionIfNotExist(ScopeType.class, typeCode);		
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("visilbe", visible);
		return get(typeCode, actorCode, visible, pageable, firstTupleIndex, numberOfTuples, removeTypeCodeFromIdentifier);
	}
	
	@Override
	public Collection<Scope> getByTypeCodeByActorCode(String typeCode, String actorCode, Boolean visible,Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples
			,Boolean removeTypeCodeFromIdentifier) {
		CodeExecutor.getInstance().throwExceptionIfNotExist(ScopeType.class, typeCode);
		CodeExecutor.getInstance().throwExceptionIfNotExist(Actor.class, actorCode);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("visilbe", visible);
		
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setQuery(new Query().setIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		queryExecutorArguments.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_TYPE_CODE,typeCode,ScopeQuerier.PARAMETER_NAME_ACTOR_CODE,actorCode
				,ScopeQuerier.PARAMETER_NAME_VISIBLE,visible);
		if(pageable == null)
			pageable = NumberHelper.isGreaterThanOrEqualZero(firstTupleIndex) || NumberHelper.isGreaterThanZero(numberOfTuples);
		if(Boolean.TRUE.equals(pageable)) {
			if(NumberHelper.isLessThanZero(firstTupleIndex))
				firstTupleIndex = 0;
			if(NumberHelper.isLessThanZero(numberOfTuples))
				numberOfTuples = 25;
			queryExecutorArguments.setFirstTupleIndex(firstTupleIndex);
			queryExecutorArguments.setNumberOfTuples(numberOfTuples);
		}
		Collection<Scope> scopes = EntityReader.getInstance().readMany(Scope.class, queryExecutorArguments);
		if(CollectionHelper.isEmpty(scopes))
			return null;
		if(Boolean.TRUE.equals(removeTypeCodeFromIdentifier))
			scopes.forEach(scope -> {
				scope.setIdentifier(StringUtils.substringAfter(scope.getIdentifier(), typeCode));
			});
		return scopes;
	}
	
	@Override
	public Collection<Scope> getSectionsByActorCode(String actorCode, Boolean visible, Boolean pageable,
			Integer firstTupleIndex, Integer numberOfTuples, Boolean removeTypeCodeFromIdentifier) {
		Collection<Scope> scopes = getByTypeCodeByActorCode(ScopeType.CODE_SECTION, actorCode, visible, pageable, firstTupleIndex, numberOfTuples, Boolean.FALSE);
		if(Boolean.TRUE.equals(removeTypeCodeFromIdentifier))
			scopes.forEach(scope -> {
				scope.setIdentifier(StringUtils.substringAfterLast(scope.getIdentifier(), ScopeType.CODE_SECTION));
			});
		return scopes;
	}
	
	@Override
	public Collection<String> getSectionsIdentifiersByActorCode(String actorCode, Boolean visible, Boolean pageable,
			Integer firstTupleIndex, Integer numberOfTuples, Boolean removeTypeCodeFromIdentifier) {
		Collection<Scope> scopes = getByTypeCodeByActorCode(ScopeType.CODE_SECTION, actorCode, visible, pageable, firstTupleIndex, numberOfTuples, Boolean.FALSE);
		if(CollectionHelper.isEmpty(scopes))
			return null;
		return scopes.stream().map(scope -> scope.getIdentifier()).collect(Collectors.toList());
	}
	
	@Override
	public Collection<Scope> getAdministrativeUnitsByActorCode(String actorCode, Boolean visible, Boolean pageable,
			Integer firstTupleIndex, Integer numberOfTuples, Boolean removeTypeCodeFromIdentifier) {
		Collection<Scope> scopes = getByTypeCodeByActorCode(ScopeType.CODE_UA, actorCode, visible, pageable, firstTupleIndex, numberOfTuples, Boolean.FALSE);
		new ScopeAdministrativeUnitSectionsReader().readThenSet(scopes, null);
		if(Boolean.TRUE.equals(removeTypeCodeFromIdentifier))
			scopes.forEach(scope -> {
				scope.setIdentifier(StringUtils.substringAfterLast(scope.getIdentifier(), ScopeType.CODE_UA));
				if(StringHelper.isBlank(scope.getSectionAsString()))
					return;
				scope.setSectionAsString(StringUtils.substringAfterLast(scope.getSectionAsString(), ScopeType.CODE_SECTION));
			});
		return scopes;
	}
	
	@Override
	public Collection<Scope> getBudgetSpecializationUnitsByActorCode(String actorCode, Boolean visible,
			Boolean pageable, Integer firstTupleIndex, Integer numberOfTuples, Boolean removeTypeCodeFromIdentifier) {
		Collection<Scope> scopes = getByTypeCodeByActorCode(ScopeType.CODE_USB, actorCode, visible, pageable, firstTupleIndex, numberOfTuples, Boolean.FALSE);
		new ScopeBudgetSpecializationUnitSectionsReader().readThenSet(scopes, null);
		if(Boolean.TRUE.equals(removeTypeCodeFromIdentifier))
			scopes.forEach(scope -> {
				scope.setIdentifier(StringUtils.substringAfterLast(scope.getIdentifier(), ScopeType.CODE_USB));
				if(StringHelper.isBlank(scope.getSectionAsString()))
					return;
				scope.setSectionAsString(StringUtils.substringAfterLast(scope.getSectionAsString(), ScopeType.CODE_SECTION));
			});
		return scopes;
	}
	
	@Override
	public Collection<Scope> getVisibleSections(String actorCode) {
		return getVisibles(actorCode, ScopeOfTypeSectionQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}

	@Override
	public Collection<Scope> getVisibleAdministrativeUnits(String actorCode) {
		return getVisibles(actorCode, ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}

	@Override
	public Collection<Scope> getVisibleBudgetSpecializationUnits(String actorCode) {
		return getVisibles(actorCode, ScopeOfTypeBudgetSpecializationUnitQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}
	
	/**/
	
	private static Collection<Scope> getVisibles(String actorCode,String queryIdentifier) {
		Collection<Scope> scopes = EntityReader.getInstance().readMany(Scope.class,new QueryExecutorArguments().setQuery(new Query().setIdentifier(queryIdentifier))
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode));
		if(CollectionHelper.isEmpty(scopes))
			return null;
		return scopes;
	}
}