package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.util.Collection;

import javax.persistence.EntityManager;
import javax.persistence.NoResultException;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.security.keycloak.server.KeycloakClientGetter;
import org.cyk.utility.security.keycloak.server.User;
import org.cyk.utility.security.keycloak.server.UserManager;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

public class ProductionCreateUsers extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void __listenAfter__() {
		super.__listenAfter__();
		KeycloakClientGetter.INSTANCE.set(null);
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "prod";
	}
	
	private void init() {
		/*
		VariableHelper.write(VariableName.KEYCLOAK_ENABLED, Boolean.TRUE);
		VariableHelper.write(VariableName.KEYCLOAK_SERVER_URL, "http://10.3.4.60:8080/auth/");
		VariableHelper.write(VariableName.KEYCLOAK_REALM_NAME, "SIIBTEST");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_IDENTIFIER, "admin-cli");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_SECRET, "ac0749e3-14f8-4a14-a6e1-4edfb9375e69");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_USERNAME, "mic-acteur-api");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_PASSWORD, "mic-@cteur-@pi@2O2o");
		*/
	}
	
	@Test
	public void run(){
		rffim();
	}
	
	private void rffim(){
		init();		
		Collection<Section> sections = EntityManagerGetter.getInstance().get().createQuery("SELECT s FROM Section s ORDER BY s.code ASC", Section.class).getResultList();	
		if(CollectionHelper.isNotEmpty(sections)) {
			for(Section section : sections) {
				String username = "RFFIM_"+section.getCode();
				createKeycloakUser(username, "RFFIM", section.getCode());
				break;
				//createActorScope(username, "SECTION", section.getCode());
			}
		}	
	}
	
	private void createKeycloakUser(String username,String scopeTypeCode,String scopeCode) {
		String password = username;
		User user = UserManager.getInstance().readByUserName(username);
		if(user != null) {
			System.out.println("L'utilisateur <<"+username+">> existe déja");
			return;
		}		
		user = new User().setName(username).setElectronicMailAddress(username+"@mail.com").setPass(password).setFirstName(username).setLastNames(scopeTypeCode+" "+scopeCode);
		UserManager.getInstance().create(user);
		user = UserManager.getInstance().readByUserName(username);
		if(user == null) {
			System.out.println("L'utilisateur <<"+username+">> a été créé");
		}else {
			System.out.println("L'utilisateur <<"+username+">> n'a pas été créé");
		}		
	}
	
	protected void createActorScope(String username,String scopeTypeCode,String scopeCode) {
		username = username.toLowerCase();
		ActorScope actorScope = null;
		try {
			actorScope = EntityManagerGetter.getInstance().get().createQuery("SELECT ass FROM ActorScope ass WHERE ass.actor.code = '"
					+username+"' AND ass.scope.type.code = '"+scopeTypeCode+"' AND ass.scope.code = '"+scopeCode+"'", ActorScope.class).getSingleResult();
		} catch (NoResultException exception) {}
		if(actorScope != null) {
			System.out.println(username+" a déja la visibilité sur "+scopeTypeCode+" " +scopeCode);
			return;
		}
		Actor actor = null;
		try {
			actor = EntityManagerGetter.getInstance().get().createQuery("SELECT a FROM Actor a WHERE a.code = '"+username+"'", Actor.class).getSingleResult();
		} catch (NoResultException exception) {}
		if(actor == null) {
			System.out.println("acteur <<"+username+">> inexistant.");
			return;
		}
		Scope scope = null;
		try {
			scope = EntityManagerGetter.getInstance().get().createQuery("SELECT s FROM Scope s WHERE s.type.code = '"+scopeTypeCode+"' AND s.code = '"+scopeCode+"'", Scope.class).getSingleResult();
		} catch (NoResultException exception) {}
		if(scope == null) {
			System.out.println("domaine <<"+scopeTypeCode+scopeCode+">> inexistant.");
			return;
		}
		actorScope = new ActorScope().setIdentifier(username+"_"+scopeTypeCode+"_"+scopeCode).setActor(actor).setScope(scope);						
		EntityManager entityManager = EntityManagerGetter.getInstance().get();
		entityManager.getTransaction().begin();
		entityManager.persist(actorScope);
		entityManager.getTransaction().commit();
		System.out.println(username+" a maintenant la visibilité sur "+scopeTypeCode+" " +scopeCode);		
	}
}