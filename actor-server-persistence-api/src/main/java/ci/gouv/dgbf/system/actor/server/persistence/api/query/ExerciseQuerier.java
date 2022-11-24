package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Exercise;

public interface ExerciseQuerier extends Querier.CodableAndNamable<Exercise> {

	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(Exercise.class, "readAllForUI");
	Collection<Exercise> readAllForUI();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Exercise> implements ExerciseQuerier,Serializable {
	
		@Override
		public Collection<Exercise> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_ALL_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readAllForUI();
			return super.readMany(arguments);
		}
		
		@Override
		public Collection<Exercise> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(Exercise.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		protected Class<Exercise> getKlass() {
			return Exercise.class;
		}
	}
	
	/**/
	
	static ExerciseQuerier getInstance() {
		return Helper.getInstance(ExerciseQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Exercise.class);
		QueryManager.getInstance().register(Query.buildSelect(Exercise.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI
				, "SELECT t.identifier,t.code,t.name FROM Exercise t ORDER BY t.code DESC")
				.setTupleFieldsNamesIndexesFromFieldsNames(Exercise.FIELD_IDENTIFIER,Exercise.FIELD_CODE,Exercise.FIELD_NAME));
	}
}