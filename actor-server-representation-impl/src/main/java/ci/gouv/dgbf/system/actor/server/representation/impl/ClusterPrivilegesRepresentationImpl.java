package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ClusterPrivilegesRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ClusterPrivilegesDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ClusterPrivilegesRepresentationImpl extends AbstractRepresentationEntityImpl<ClusterPrivilegesDto> implements ClusterPrivilegesRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
