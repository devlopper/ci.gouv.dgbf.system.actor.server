package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.LocalityPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class LocalityPersistenceImpl extends AbstractPersistenceEntityImpl<Locality> implements LocalityPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}