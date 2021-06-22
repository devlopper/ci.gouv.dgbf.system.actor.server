package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

public class ActorPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void readDynamicOne(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC_ONE));
		queryExecutorArguments.addFilterFieldsValues(ActorQuerier.PARAMETER_NAME_CODE,"christian");
		Actor actor = EntityReader.getInstance().readOne(Actor.class, queryExecutorArguments);
		assertThat(actor).isNotNull();
		assertThat(actor.getCode()).isEqualTo("christian");
	}
}