package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

@Queries(value = {
	@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Section.class,name = SectionQuerier.QUERY_NAME_READ_ALL,value = "SELECT t FROM Section t ORDER BY t.code ASC")
})
public interface SectionQuerier extends Querier.CodableAndNamable<Section> {

	/* read order by code ascending */
	String QUERY_NAME_READ_ALL = "readAll";
	String QUERY_IDENTIFIER_READ_ALL = QueryIdentifierBuilder.getInstance().build(Section.class, QUERY_NAME_READ_ALL);
	Collection<Section> read();
	
	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(Section.class, "readAllForUI");
	Collection<Section> readAllForUI();
	
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_ALL);
	Long count();
	
	String QUERY_IDENTIFIER_READ_VISIBLES_BY_ACTOR_CODE_FOR_UI = QueryIdentifierBuilder.getInstance().build(Section.class, "readVisiblesByActorCodeForUI");
	Collection<Section> readVisiblesByActorCodeForUI(String actorCode);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Section> implements SectionQuerier,Serializable {
		@Override
		public Collection<Section> read() {
			return QueryExecutor.getInstance().executeReadMany(Section.class, QUERY_IDENTIFIER_READ_ALL);
		}
		
		@Override
		public Collection<Section> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(Section.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		public Collection<Section> readVisiblesByActorCodeForUI(String actorCode) {
			if(StringHelper.isBlank(actorCode))
				return null;
			Collection<Scope> scopes =  ScopeOfTypeSectionQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
					.setQueryFromIdentifier(ScopeOfTypeSectionQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER)
					.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode));
			//Collection<Scope> scopes =  QueryExecutor.getInstance().executeReadMany(Scope.class, ScopeOfTypeSectionQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER
			//		,ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode);
			if(CollectionHelper.isEmpty(scopes))
				return null;
			return scopes.stream().map(x -> new Section().setIdentifier(x.getIdentifier()).setCode(x.getCode()).setName(x.getName())).collect(Collectors.toList());
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Collection<Section> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_ALL.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_ALL_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readAllForUI();
			if(QUERY_IDENTIFIER_READ_VISIBLES_BY_ACTOR_CODE_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readVisiblesByActorCodeForUI((String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE));
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
		@Override
		protected Class<Section> getKlass() {
			return Section.class;
		}
	}
	
	/**/
	
	static SectionQuerier getInstance() {
		return Helper.getInstance(SectionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Section.class);
		QueryHelper.addQueries(
				Query.buildSelect(Section.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI, "SELECT t.identifier,t.code,t.name FROM Section t ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(Section.FIELD_IDENTIFIER,Section.FIELD_CODE,Section.FIELD_NAME)
		);		
	}
}