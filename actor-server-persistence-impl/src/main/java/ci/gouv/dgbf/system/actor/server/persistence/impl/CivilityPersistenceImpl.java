package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.CivilityPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class CivilityPersistenceImpl extends AbstractPersistenceEntityImpl<Civility> implements CivilityPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}