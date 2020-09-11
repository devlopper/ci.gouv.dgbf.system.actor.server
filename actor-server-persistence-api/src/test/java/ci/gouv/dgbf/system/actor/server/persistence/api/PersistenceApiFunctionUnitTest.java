package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierGetter;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

public class PersistenceApiFunctionUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		SecurityHelper.PRINCIPALABLE.set(Boolean.FALSE);
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void read(){
		assertThat(EntityReader.getInstance().readMany(Function.class,FunctionQuerier.QUERY_IDENTIFIER_READ)
				.stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1.1","1.2","2.1","2.2","2.3");
	}
	
	@Test
	public void count(){
		assertThat(EntityCounter.getInstance().count(Function.class,FunctionQuerier.QUERY_IDENTIFIER_COUNT)).isEqualTo(5l);
	}
	
	@Test
	public void readWhereCodeOrNameLike(){
		assertThat(EntityReader.getInstance().readMany(Function.class,QueryIdentifierGetter.getInstance().get(Function.class, QueryName.READ_WHERE_CODE_OR_NAME_LIKE)
				,FunctionQuerier.PARAMETER_NAME_CODE,"1.2",FunctionQuerier.PARAMETER_NAME_NAME,"1.2")
				.stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1.2");
	}
	
	@Test
	public void countWhereCodeOrNameLike(){
		assertThat(EntityCounter.getInstance().count(Function.class,QueryIdentifierGetter.getInstance().get(Function.class, QueryName.COUNT_WHERE_CODE_OR_NAME_LIKE)
				,FunctionQuerier.PARAMETER_NAME_CODE,"1.2",FunctionQuerier.PARAMETER_NAME_NAME,"1.2"))
			.isEqualTo(1l);
	}
	
	@Test
	public void readWithProfiles(){
		assertThat(EntityReader.getInstance().readMany(Function.class,FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_PROFILES)
				.stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1.1","1.2","2.1","2.2","2.3");
	}
	
	@Test
	public void readByTypes(){
		assertThat(EntityReader.getInstance().readMany(Function.class,FunctionQuerier.QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,FunctionQuerier.PARAMETER_NAME_TYPES_CODES,List.of("1"))
				.stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1.1","1.2");
		assertThat(EntityReader.getInstance().readMany(Function.class,FunctionQuerier.QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,FunctionQuerier.PARAMETER_NAME_TYPES_CODES,List.of("2"))
				.stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2.1","2.2","2.3");
		assertThat(EntityReader.getInstance().readMany(Function.class,FunctionQuerier.QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,FunctionQuerier.PARAMETER_NAME_TYPES_CODES,List.of("a"))).isNull();
		assertThat(EntityReader.getInstance().readMany(Function.class,FunctionQuerier.QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,FunctionQuerier.PARAMETER_NAME_TYPES_CODES,List.of("1","2"))
				.stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1.1","1.2","2.1","2.2","2.3");
	}
	
	@Test
	public void countByTypes(){
		assertThat(EntityCounter.getInstance().count(Function.class,FunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,FunctionQuerier.PARAMETER_NAME_TYPES_CODES,List.of("1"))).isEqualTo(2l);
		assertThat(EntityCounter.getInstance().count(Function.class,FunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,FunctionQuerier.PARAMETER_NAME_TYPES_CODES,List.of("2"))).isEqualTo(3l);
		assertThat(EntityCounter.getInstance().count(Function.class,FunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,FunctionQuerier.PARAMETER_NAME_TYPES_CODES,List.of("a"))).isEqualTo(0l);
		assertThat(EntityCounter.getInstance().count(Function.class,FunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,FunctionQuerier.PARAMETER_NAME_TYPES_CODES,List.of("1","2"))).isEqualTo(5l);
	}
	
	@Test
	public void readWithProfilesByTypes(){
		assertThat(EntityReader.getInstance().readMany(Function.class,new QueryExecutorArguments().setQuery(new Query()
				.setIdentifier(FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_PROFILES_BY_TYPES_CODES))
				.addFilterField(FunctionQuerier.PARAMETER_NAME_TYPES_CODES,List.of("1")))
				.stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1.1","1.2");
	}
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(new FunctionType().setCode("1").setName("1"),new FunctionType().setCode("2").setName("1"));
		EntityCreator.getInstance().createManyInTransaction(
				new Function().setCode("1.1").setName("1").setTypeFromIdentifier("1")
				,new Function().setCode("1.2").setName("1").setTypeFromIdentifier("1")
				,new Function().setCode("2.1").setName("2").setTypeFromIdentifier("2")
				,new Function().setCode("2.2").setName("2").setTypeFromIdentifier("2")
				,new Function().setCode("2.3").setName("2").setTypeFromIdentifier("2")
				);
		
		EntityCreator.getInstance().createManyInTransaction(new ProfileType().setCode("1").setName("1"),new ProfileType().setCode("2").setName("2"));
		EntityCreator.getInstance().createManyInTransaction(
				new Profile().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier("1")
				,new Profile().setCode("3").setName("3").setTypeFromIdentifier("1")
				,new Profile().setCode("4").setName("3").setTypeFromIdentifier("1")
				,new Profile().setCode("5").setName("3").setTypeFromIdentifier("1")
			);
		
		EntityCreator.getInstance().createManyInTransaction(
				new ProfileFunction().setProfileFromIdentifier("1").setFunctionFromIdentifier("1.1")
				,new ProfileFunction().setProfileFromIdentifier("2").setFunctionFromIdentifier("2.2")
				,new ProfileFunction().setProfileFromIdentifier("3").setFunctionFromIdentifier("2.2")
				);
	}
}