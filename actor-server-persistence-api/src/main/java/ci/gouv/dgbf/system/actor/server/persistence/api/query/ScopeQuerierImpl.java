package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class ScopeQuerierImpl extends ScopeQuerier.AbstractImpl {

	@Override
	public Collection<Scope> readByTypeCodeByActorCode(String typeCode, String actorCode, Boolean visible,Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples
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
	
}
