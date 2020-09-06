package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

@Queries(value = {
	@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Section.class,name = SectionQuerier.QUERY_NAME_READ,value = "SELECT t FROM Section t ORDER BY t.code ASC")
	//,@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Function.class,name = FunctionQuerier.QUERY_NAME_COUNT,value = "SELECT COUNT(t.identifier) FROM Function t")
})
public interface SectionQuerier extends Querier.CodableAndNamable<Section> {

	/* read order by code ascending */
	String QUERY_NAME_READ = "read";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Section.class, QUERY_NAME_READ);
	
	String QUERY_NAME_COUNT = "count";
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Section> implements SectionQuerier,Serializable {
			
	}
	
	/**/
	
	static SectionQuerier getInstance() {
		return Helper.getInstance(SectionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Section.class);
		
	}
}