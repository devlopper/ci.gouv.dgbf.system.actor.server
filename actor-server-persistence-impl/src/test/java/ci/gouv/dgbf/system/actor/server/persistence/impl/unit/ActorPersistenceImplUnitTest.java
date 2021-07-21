package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import java.util.Collection;

import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Field;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

public class ActorPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "actors";
	}
	
	@Test
	public void actors(){
		assertActors(null, null, null
		, new String[] {Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION}
		, new Object[][] { 
			{"1","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
			,{"2","Monsieur","Admin","Administrateur",null, "admin@mail.com", null, null, null}
			,{"3","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail02.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
		}, Boolean.TRUE);
	}
	
	@Test
	public void actors_filterByCodeLike(){
		assertActors("c", null, null
		, new String[] {Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION}
		, new Object[][] { 
			{"1","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
			,{"3","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail02.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
		}, Boolean.TRUE);
		
		assertActors("a", null, null
				, new String[] {Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION}
				, new Object[][] { 
					{"1","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
					,{"2","Monsieur","Admin","Administrateur",null, "admin@mail.com", null, null, null}
					,{"3","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail02.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
				}, Boolean.TRUE);
		
		assertActors("ad", null, null
				, new String[] {Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION}
				, new Object[][] { 
				{"2","Monsieur","Admin","Administrateur",null, "admin@mail.com", null, null, null}
				}, Boolean.TRUE);
		
	}
	
	@Test
	public void actors_filterByCodeEqual(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		queryExecutorArguments.getFilter(Boolean.TRUE).addFields(new Field().setName(ActorQuerier.PARAMETER_NAME_CODE).setValue("christian"));
		queryExecutorArguments.addProcessableTransientFieldsNames(
				Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION);
		Collection<Actor> actors = EntityReader.getInstance().readMany(Actor.class, queryExecutorArguments);
		assertActors(actors, new Object[][] { 
			{"1","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
		}, Boolean.TRUE);
	}
	
	@Test
	public void actors_filterByFirstNameLike(){
		assertActors(null, "k", null
		, new String[] {Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION}
		, new Object[][] { 
			{"1","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
			,{"3","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail02.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
		}, Boolean.TRUE);
		
		assertActors(null, "a", null
				, new String[] {Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION}
				, new Object[][] { 
					{"1","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
					,{"2","Monsieur","Admin","Administrateur",null, "admin@mail.com", null, null, null}
					,{"3","Monsieur","Komenan","Yao Christian","Fonctionnaire", "kycdev@gmail02.com", "DTI Direction des traitements informatiques", "327 Budget", "Chef de service"}
				}, Boolean.TRUE);
		
		assertActors(null, "ad", null
				, new String[] {Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION}
				, new Object[][] { 
				{"2","Monsieur","Admin","Administrateur",null, "admin@mail.com", null, null, null}
				}, Boolean.TRUE);
		
	}
	
}